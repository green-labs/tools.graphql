// type CompletionArgs = {
//   breakingChanges: Change[]
//   dangerousChanges: Change[]
//   nonBreakingChanges: Change[]
// }

// Change는 아래 링크의 타입 참조
// https://github.com/kamilkisiela/graphql-inspector/blob/306ab93473c8306d1312404abacfab2efd8aa793/packages/core/src/diff/changes/change.ts

const { Octokit } = require("octokit")
const { fetch } = require("@whatwg-node/fetch")

const SLACK_URL = 'https://hooks.slack.com/services/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'
const GITHUB_OWNER = 'green-labs'
const GITHUB_REPO = 'farmmorning-backend'

const inputOrArgsChanges = ['FIELD_ARGUMENT_ADDED', 'INPUT_FIELD_ADDED']
const defaultDirectives = ['@deprecated', '@specifiedBy']
const client = new Octokit({
    auth: process.env.GITHUB_TOKEN,
})
const parsePullRequestId = githubRef => {
    const result = /refs\/pull\/(\d+)\/merge/g.exec(githubRef)
    if (!result) throw new Error("Reference not found.")
    const [, pullRequestId] = result
    return pullRequestId
}
const pullRequestId = parsePullRequestId(process.env.GITHUB_REF)
const ref = {
    owner: GITHUB_OWNER,
    repo: GITHUB_REPO,
    issue_number: pullRequestId
}

function quotesTransformer(msg, symbols = '**') {
    const findSingleQuotes = /'([^']+)'/gim
    const findDoubleQuotes = /"([^"]+)"/gim

    function transformm(_, value) {
        return `${symbols}${value}${symbols}`
    }

    return msg.replace(findSingleQuotes, transformm).replace(findDoubleQuotes, transformm)
}

function slackCoderize(msg) {
    return quotesTransformer(msg, '`')
}

function renderAttachments({
    changes,
    title,
    color,
}) {
    const text = changes.map(change => slackCoderize(change.message)).join('\n')

    return {
        mrkdwn_in: ['text', 'fallback'],
        color,
        author_name: title,
        text,
        fallback: text,
    }
}

function createAttachments(changes) {
    const breakingChanges = changes.breakingChanges
    const dangerousChanges = changes.dangerousChanges
    const safeChanges = changes.nonBreakingChanges
    const attachments = []

    if (breakingChanges.length) {
        attachments.push(
            renderAttachments({
                color: '#E74C3B',
                title: 'Breaking changes',
                changes: breakingChanges,
            }),
        )
    }

    if (dangerousChanges.length) {
        attachments.push(
            renderAttachments({
                color: '#F0C418',
                title: 'Dangerous changes',
                changes: dangerousChanges,
            }),
        )
    }

    if (safeChanges.length) {
        attachments.push(
            renderAttachments({
                color: '#23B99A',
                title: 'Safe changes',
                changes: safeChanges,
            }),
        )
    }

    return attachments
}

function pluralize(word, num) {
    return word + (num > 1 ? 's' : '')
}

async function notifyWithSlack({
    url,
    changes,
    environment,
    repo,
    owner,
    commentId,
}) {
    const totalChanges = changes.breakingChanges.length + changes.dangerousChanges.length + changes.nonBreakingChanges.length
    const schemaName = environment ? `${environment} schema` : `schema`
    const sourceLink = commentId
        ? ` (<https://github.com/${owner}/${repo}/pull/${pullRequestId}#issuecomment-${commentId}>)`
        : ''

    const event = {
        username: 'GraphQL Inspector',
        icon_url: 'https://graphql-inspector/img/logo-slack.png',
        text: `:male-detective: Hi, I found *${totalChanges} ${pluralize(
            'change',
            totalChanges,
        )}* in ${schemaName}${sourceLink}:`,
        attachments: createAttachments(changes),
    }

    await fetch(url, {
        method: 'POST',
        headers: {
            'content-type': 'application/json',
        },
        body: JSON.stringify(event),
    })
}

const onComplete = async (changes) => {
    changes = {
        ...changes,
        nonBreakingChanges: changes.nonBreakingChanges.filter(c =>
            c.type === 'DIRECTIVE_ADDED' && defaultDirectives.includes(c.name)
        )
    }

    const messages = []
    const inputChanges = changes.dangerousChanges
        .filter((dc => inputOrArgsChanges.includes(dc.type)))

    if (changes.breakingChanges.length) {
        messages.push('### Breaking Changes ⚠️')
        messages.push(...changes.breakingChanges.map(c => `- ${c.message}`))
        messages.push('')
        messages.push('Please consult to @green-labs/frontend')
        process.exitCode = 1
    }

    if (changes.dangerousChanges.length) {
        messages.push('### Dangerous Changes')
        messages.push(...changes.dangerousChanges
            .filter((dc => !inputOrArgsChanges.includes(dc.type)))
            .map(c => `- ${c.message}`))
    }

    if (inputChanges.length) {
        messages.push('#### Input or Argument Changes (Be prepared for broken frontend builds)')
        messages.push(...inputChanges.map(c => `- ${c.message}`))
    }

    const labelsRes = await client.rest.issues.listLabelsOnIssue(ref)
    const commentsRes = await client.rest.issues.listComments(ref)
    const intentionalBreaking = labelsRes.data.filter(l => l.name === 'breaking-change').length
    const botComments = commentsRes.data.filter(d => d.user.type === 'Bot' && d.user.login === 'github-actions[bot]')
    const prevComment = botComments[botComments.length - 1]
    const comment = messages.join('\n').replaceAll('\'', '`')
    const isCommentChanged = prevComment && prevComment.body !== comment

    console.debug('isCommentChanged: ' + isCommentChanged + ' botComments: ' + botComments.length)

    if (intentionalBreaking) {
        process.exitCode = 0
    }

    // COND: (new comment exists) AND (no bot comments OR previous comment is not same on current comment)
    if (!intentionalBreaking && comment && (botComments.length === 0 || isCommentChanged)) {
        const created = await client.rest.issues.createComment({
            ...ref,
            body: comment
        })

        notifyWithSlack({
            url: SLACK_URL,
            changes,
            environment: undefined,
            owner: GITHUB_OWNER,
            repo: GITHUB_REPO,
            commentId: created.data.id,
        })
    }
}

module.exports = onComplete
