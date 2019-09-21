if $1 == 'help'; then 

        echo "usage <program> housenumber (eg: 13) postalcode: (eg: 1931 GL) year month day\n";
fi 

curl -XGET "https://mijn.pwn.nl/api/consumption/GetAansluitingen" -e "https://mijn.pwn.nl/MijnPwn/MoveInMoveOut/MoveInNewClient" -H "Content-Type: application/json" -vv -d"{\"houseNumber\":\"$1\",\"houseNumberAddittion\":\"\",\"postCode\":\"$2\",\"level\":\"767\",\"date\":\"$3-$4-$5T00:00:00.000Z\",\"originalDate\":\"$5-$4-$3\"}"
