# Contributing to Meck

:+1::tada: First off, thanks for taking the time to contribute! :tada::+1:

The following is a set of guidelines for contributing to Meck. These are just
guidelines, not rules, use your best judgment and feel free to propose changes
to this document in a pull request.

## What should I know before I get started?

### Code of Conduct

This project adheres to the Contributor Covenant
[code of conduct](CODE_OF_CONDUCT.md). By participating, you are expected to
uphold this code. Please report unacceptable behavior to
[meck at alind dot io].

## How Can I Contribute?

You can:

* Submit an issue (see [Reporting Bugs](#reporting-bugs))
* Suggest an enhancement (see [Suggesting Enhancements](#suggesting-enhancements))
* Submit a pull request (see [Pull Requests](#pull-requests))

### Your First Contribution

If you want to contribute with documentation, wiki pages, examples, code or
tests and are unsure of how to start, just open an issue to start a discussion.
It could be a proposal, a question for support or further direction or any
other feedback you might have.

### Reporting Bugs

Submitting a good bug report will help identifying, debugging and solving an
issue.

Please check first if any similar issues have already been reported. If so,
add to the discussion by commenting on one of those instead.

When you're ready to submit a bug report you can use the
[bug report template](.github/ISSUE_TEMPLATE.md) defined in the project (it's
automatically used whenever you create a new issue on GitHub). Make sure to
fill in which versions you are using and instructions of how to reproduce the
problem.

### Suggesting Enhancements

Suggesting enhancements doesn't have to be as structured as a bug report, but
should still contain the motivation for the enhancement, an example use case
and some reasoning why this enhancement should go into Meck itself (reasons it
should not might include that it works as an external library, that it is more
test framework related than mocking related etc.)

### Pull Requests

Pull requests really appreciated. To increase the chance of getting your code
merged, make sure the pull request is small and well structured. You should
prepare your pull request to try to meet the following guidelines where it
makes sense:

1. Squash all changes into one commit. If you have many independent changes,
   submit each in its own pull request.
2. Document any external API functions changed or added via EDoc.
3. Run the existing tests to make sure you didn't break anything.
3. Add working tests that illustrate and cover the changes, or detects an issue
   to be fixed. A good example is to create a failing test case that exposes
   the issue you are trying to fix, before fixing it.
4. Make sure the code and commit follow the [style guides](#styleguides).
5. (Optional) Add type specifications and run Dialyzer where it makes sense.

## Styleguides

### Commit Messages

Commit messages should be limited to 50 characters without a period on the
subject line and be written in imperative mood.

Longer explanations should be in the body, two lines below the message, wrapped
at 72 characters.

See [How to Write a Git Commit Message](http://chris.beams.io/posts/git-commit/).

### Code

* Lines should be no longer than 80 characters. This is isn't some arbitrary
  length based on nostalgia, it's just a choice of fitting limit if you want to
  have several files open at once next to each other on a modern wide screen
  monitor.
* Functions should be exported one by one in their own export statement. This
  is so that one export can easily be rearranged or removed without messing
  with commas and Erlang attributes.

If you are unsure, try to find some similar code already in the repository and
mimic that. Otherwise just submit the pull request to get some stylistic
feedback if necessary.
