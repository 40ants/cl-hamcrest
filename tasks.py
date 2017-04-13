import os
from invoke import task, run



@task
def build_docs(ctx):
    def git(cmd):
        return run('cd docs/build/html && git {0}'.format(cmd))

    # build docs
    run('cd docs && make html')

    # if no git repository in docs/build/html,
    # then init one
    if not os.path.exists('docs/build/html/.git'):
        result = run("git remote -v | grep '^origin.*(push)$'")

        origin = result.stdout.strip().split()[1]
        git('init')
        git('remote add origin {0}'.format(origin))

    git('add .')
    git('commit -m "Update docs"')
    git('push --force origin master:gh-pages')
