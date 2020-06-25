#!/usr/bin/expect -f

set PASSWD [exec aws ssm get-parameter --name /personal/edgarklerks/ad-passwd --with-decryption | jq -r ".\[\]|.Value" ] 


set TOKEN [lindex $argv 0] 



spawn openconnect --user media\\E.Klerks  https://vpn-nl.sanoma.com/OTP

expect "Password" { 
  send "$PASSWD\r"
}

expect "Response" {
  send "$TOKEN\r"
}
interact


