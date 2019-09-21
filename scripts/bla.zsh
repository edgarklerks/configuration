[ -e ~/.ssh/config.d ] && find ~/.ssh/config.d -print -quit | grep -q . && (
        #newestconfig=$(ls -t ~/.ssh/config.d/ | head -1)
        newestconfig=$(find ~/.ssh/config.d/* -printf '%T+ %p\n' | sort -r | head -n1 | awk '{print $2}')
        if [ "$newestconfig" -nt ~/.ssh/config ]; then
            [ -e ~/.ssh/config ] && mv ~/.ssh/config ~/.ssh/config.bak.$(date -Ins)
            find ~/.ssh/config.d/* -type f -print0 | sort -z | xargs -0 -n1 cat > ~/.ssh/config
        fi
)
