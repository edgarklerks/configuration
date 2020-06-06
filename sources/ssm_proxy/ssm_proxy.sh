#!/bin/bash 


source ~/envs/awscli/bin/activate

PUBKEY="$(cat ~/.ssh/id_ed25519.pub)"

connect_command="$(cat <<DOC 
sudo sed -i -E 's|(.*)AuthorizedKeysFile(.*)|AuthorizedKeysFile /etc/ssh_user|' /etc/ssh/sshd_config; echo $PUBKEY | sudo tee /etc/ssh_user; sudo service sshd restart 
DOC
)"

disconnect_command="$(cat <<DOC 
sudo sed -i -E 's|(.*)AuthorizedKeysFile(.*)|AuthorizedKeysFile none|' /etc/ssh/sshd_config; sudo rm /etc/ssh_user; sudo service sshd restart 
DOC
)"



TARGET="$1"
PORTNUMBER="$2"
NAME="$(echo "$TARGET" | sed -E 's/^tag-//')"

INSTANCE_ID="$(
aws ec2 describe-instances  | jq -c ".Reservations | .[] | .Instances | .[] | select(.Tags | try map(.Value | test(\"$NAME\")) | any)" | while read line; do 
  STATE="$(echo "$line" | jq -r ".State.Name")"
  NAME="$(echo "$line" | jq -r ".Tags | .[] |  select(.Key == \"Name\") | .Value")"
  IP="$(echo "$line" | jq -r ".PrivateIpAddress")"
  InstanceId="$(echo "$line" | jq -r ".InstanceId")"
  if [[ "$STATE" == "terminated" ]]; then 
    continue
  fi 

  echo "$InstanceId"
  exit 
done 
)"


function cleanup(){

echo "closing down" > /tmp/closedown
aws ssm send-command \
    --document-name "AWS-RunShellScript" \
    --parameters commands="[\"$disconnect_command\"]" \
    --targets "Key=instanceids,Values=$INSTANCE_ID" \
    --comment "disconnect ssh"  >> /tmp/closedown.log 2>&1


}

trap cleanup EXIT

aws ssm send-command \
    --document-name "AWS-RunShellScript" \
    --parameters commands="[\"$connect_command\"]" \
    --targets "Key=instanceids,Values=$INSTANCE_ID" \
    --comment "connect ssh" >/dev/null 2>&1 

aws ssm start-session --target "$INSTANCE_ID" --document-name AWS-StartSSHSession --parameters "portNumber=$PORTNUMBER"

