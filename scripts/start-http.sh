#!/usr/bin/zsh
VPC_ID="vpc-7d482e18"
GROUP_NAME="BastionHTTPEpklerks"
NETWORK_IF_ID="eni-045f267d"
IFS=" "

function get_security_group_id(){
    name="$1"
    aws --output text ec2 describe-security-groups | grep "$name" | awk -F'\t' '{print $3}'
}

function destroy_security_group(){
    name="$1"
    sg_id="$(get_security_group_id "$GROUP_NAME")"
    if [[ -z "$sg_id" ]]; then 
            printf "sg_id empty, no such security group\n"
            return 0
    fi 
            GROUPS=($(get_security_groups "$NETWORK_IF_ID" | sed "s/$sg_id//"))
        printf "Destroying %s with id %s on network dev %s and groups %s\n" $name "$sg_id" "$NETWORK_IF_ID" "${GROUPS[*]}"
        if [[ -z "${GROUPS[@]}" ]]; then 
    aws --output text ec2 delete-security-group --group-id $(get_security_group_id "$name")
    else 

    aws --output text ec2 modify-network-interface-attribute --network-interface-id "$NETWORK_IF_ID" --groups "${GROUPS[@]}"
    aws --output text ec2 delete-security-group --group-id $(get_security_group_id "$name")
        fi 
}

function get_security_groups(){
    network_id="$1"
    aws --output text ec2 describe-network-interface-attribute --attribute groupSet --network-interface-id "$network_id" | tail -n+2 | awk -F'\t' '{print $2}' | tr '\n' ' '
}

function get_ip(){
    curl http://ipecho.net/plain
}

function create_security_group(){
    name="$1"
    sg_id="$(aws --output text ec2 create-security-group --group-name "$name" --description "Temporary-group" --vpc-id "$VPC_ID")"
    aws ec2 authorize-security-group-ingress --group-id "$sg_id" --protocol "tcp" --port 22 --cidr "$(get_ip)/32"
    GROUPS=($(get_security_groups "$NETWORK_IF_ID"))
    printf "Created %s with %s modifying %s and %s\n" $name $sg_id $NETWORK_IF_ID "${GROUPS[*]}" 
    aws ec2 modify-network-interface-attribute --network-interface-id "$NETWORK_IF_ID" --groups "${GROUPS[@]}" $sg_id
}
STATE="$1"
case $STATE in
    up)
	destroy_security_group "$GROUP_NAME"
	create_security_group "$GROUP_NAME"
	;;
    down)
	destroy_security_group "$GROUP_NAME"
	;;
esac
