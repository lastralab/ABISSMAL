# Github Collaboration


For full documentation visit our [wiki](https://github.com/lastralab/ABISSMAL/wiki)

## Gitflow
![abissmal gitflow](https://user-images.githubusercontent.com/22894897/150058257-d3c12516-95c2-45c8-b1d3-2591c44f5616.png)

## Collaborators

* Set up a local replicate of the tracking system that is separate from the code owner
* Make sure that issues that require modifying the same file are merged to avoid conflicts downstream (copy and paste into a single issue, close the other). Alternatively, work on these issues one at a time (after one issue was merged to main)
* For every issue that requires developing or modifying code, create a local branch with the issue number (e.g. PCT-10 for issue #10) from the main branch (e.g. see Git workflow above)
    * `git checkout main`
    * `git branch` to confirm that you're on the main branch
    * `git fetch`
    * `git pull`
    * `git checkout -b PCT-[insert issue number]`
    *  `git branch` to confirm that you're on the new local branch
* Make changes on this local branch
* Test changes on the local branch by running the tracking system and checking for anticipated outcomes or errors
* Commit local changes and push them to the same branch in the remote master (may need to publish the local branch remotely)
    * `git fetch`
    * `git pull`
    * `git add ./[name of files(s)]`
    * `git commit -m '[PCT-000] Then add your message'`
    * `git push`
* Make a pull request to merge the remote version of the local branch with the staging branch
* The code owner tests the updates on their local tracking system (see above) before confirming the pull request and merging the changes to the main branch

<h2>Setup Git credentials before cloning</h2>

These steps will save time instead of entering your Git credentials over and over.

1. Create a new <a href="https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token" target="_blank">github token</a> and store it safely. <br />
2. Store settings locally: <br />
   `git config --global credential.helper store`
3. Clone repository:<br />
   `git clone https://github.com/lastralab/Abissmal.git` <br />
4. Insert credentials using your new token as password <br />

