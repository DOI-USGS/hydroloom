# Prose — rules for every sentence a reader sees

Applies to README, articles, the vocabulary's `plain` and `definition` fields,
`remedies.json` messages, changelog fragments, doctests, docstring substitutions, PR
bodies, and milestone Summaries. Code comments and findings tables follow it where they
hold a sentence. Reviewers cite a rule here as `prose <heading>`.

## Who writes

Existing roxygen, vignette, and name-definition text is reused, not rewritten; the only
transformation is the A-13 substitution table, reviewed once as its own Log item. Agents
author prose only in changelog fragments, doctests, and PR bodies. Everything else a
user reads is maintainer-authored and agent-reviewed.

## Term, then gloss

Use the precise domain term and make it land with a short gloss or clause on first use,
then use it freely: "catchments (land areas where water collects)". Do not swap in a
looser synonym. Users meet flowlines, confluences, divergences, headwaters, and outlets;
nodes, edges, components, and bridges appear in user prose only beside the hydrographic
term. Functions go by their shared snake_case name; language syntax stays inside tabs.

## Function before definition

Say what a thing does before saying what it is. A paragraph asserts, supports with the
concrete case, then bounds with the limit or trade-off. Link paragraphs by carrying a
term forward, not by a transition word.

## Claims are earned

A sentence states what is true of the case at hand and is backed by the particulars
before or after it. No freestanding maxims, no balanced antitheses ("X is the asset; Y is
not"), no sentence that assigns an abstract role to a package or program, nothing that
would read as a poster line out of context. Hedge only what the content leaves open; one
qualifier, placed once.

## Plain over evocative

Where two words are equally precise, take the one with less metaphor: "underpinning" not
"substrate", "separating" not "pulling apart".

## Shape

Short declarative, then the longer sentence that extends it. Rarely past 50 words. No
fragments for emphasis. Prose with inline enumeration where prose works; a numbered list
only for ordered or countable items, introduced by a clause and a colon. Em-dashes and
parentheticals are fine when they carry a gloss, a scope note, or a pivot. No bold inside
running prose. No closing summary; a section ends when its content does.

## Avoid

Words: utilize, leverage (verb), robust, paradigm (loose), straightforward, delve,
unpack, landscape (as metaphor), exciting, powerful, game-changing, seamless. "Name" as
a verb for refer to or list; use identifies, lists, gives, describes, points to. "Exactly"
for emphasis.

Phrases: it is important to note, it should be noted, obviously, clearly, interestingly,
notably, furthermore, moreover, in conclusion, moving forward, at the end of the day, the
key takeaway, what's worth noting, "However" opening a sentence.

Patterns: empty intensifiers (very important, highly significant); narrating why a point
matters before making it; "No X has..." as evidence of absence; opening affirmations;
"I think" as a hedge; enthusiasm about the software.

## Error messages (`remedies.json`)

Three parts, in order: what is wrong, which rows, the one call that fixes it. Plain words
and the hydrographic term; no graph vocabulary standing alone.
