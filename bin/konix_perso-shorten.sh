#!/bin/bash -x

. "${KONIX_LIB_DIR}/lib_bash.sh"
# init
pushd "$KONIX_PERSO_DIR"
# save current state
git-freeze.sh

end(){
    git co master
}

# keep only the last 100 commits
git branch -D latest_commit
git co HEAD~100 -b latest_commit || end
git reset --soft bottom || end
git commit -m "squashed from bottom to latest_commit"
FIRST_COMMIT=`git what-commit.sh`
git rebase latest_commit master --onto "$FIRST_COMMIT"
end
