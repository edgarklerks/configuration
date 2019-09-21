if $1 == 'help'; then 

        echo "usage <program> housenumber (eg: 2) postalcode: (eg: 1733 AC) year month day\n";
fi 

curl -XPOST "https://mijn.pwn.nl/api/consumption/ValidateMeterLevel" -e "https://mijn.pwn.nl/MijnPwn/MoveInMoveOut/MoveInNewClient" -H "Content-Type: application/json" -vv -d"{\"houseNumber\":\"$1\",\"houseNumberAddittion\":\"\",\"postCode\":\"$2\",\"level\":\"767\",\"date\":\"$3-$4-$5T00:00:00.000Z\",\"originalDate\":\"$5-$4-$3\"}"
