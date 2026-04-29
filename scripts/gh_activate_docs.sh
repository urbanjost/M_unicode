#!/bin/bash
gh api --method POST repos/urbanjost/M_unicode/pages --field "source[branch]"="main" --field "source[path]"/="/docs"
gh api --method PUT  repos/urbanjost/M_unicode/pages --field "source[branch]"="main" --field "source[path]"/="/docs"
exit

AI Overview
Creating a GitHub Pages site

To enable GitHub Pages from the /docs directory using the gh CLI tool,
you must use the gh api command to interact with the GitHub REST API,
as there is no direct gh pages command currently built-in to the main CLI.

Prerequisites

    You must have the GitHub CLI installed and authenticated on your system.
    The repository must be public for GitHub Pages to be free. 

Command to Enable GitHub Pages

Navigate to your local repository's directory in your terminal and
run the following command. This command sets the publishing source for
your repository to the docs folder on your default branch (e.g., main
or master).
bash

gh api --method POST repos/{owner}/{repo}/pages --field "source[branch]"="main" --field "source[path]"/="docs"

Replace {owner} with your GitHub username or organization name, and {repo}
with the name of your repository. The main branch should be replaced if
your default branch has a different name.
Alternative using a PUT request

If the Pages site has already been created (even with default settings),
you would use a PUT request to update the source, rather than a POST
request to create it:

bash

gh api --method PUT repos/{owner}/{repo}/pages --field "source[branch]"="main" --field "source[path]"/="docs"

After Enabling

    Ensure that an index.html file (or index.md/README.md) exists in
    the /docs directory of your repository.

    GitHub will automatically run a workflow to build and deploy your
    site. You can monitor the progress in the Actions tab of your
    repository on GitHub.

    Once deployed, the URL for your site can be found in the repository's
    Settings -> Pages section or in the "About" section on the right
    side of the repository home page.

