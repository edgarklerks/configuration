#!/usr/bin/zsh -e 
account="${1-storage}"

case $account in 
        cda)
                key="totp-storage"
                ;;
        storage)
                key="totp-datastorage"
                ;;
        services)
                key="totp-dataservices"
                ;;
        bigdata)
                key="totp-newaccount"
                ;;
        *)
                echo "wrong account" 1>&2 
                exit 1
                ;;
esac

given_code="$(perl ~/scripts/openstore.pl get "$key")"
old_code=""
while true; do 
        new_code="$(oathtool --totp --base32 "$given_code")"
        if [[ "$old_code" != "$new_code" ]]; then 
                old_code="$new_code"
                echo "$new_code"
        fi 
        sleep 1
done 
