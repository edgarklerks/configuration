#!/usr/bin/expect -f


set TOKEN [lindex $argv 0] 

puts "Login in with $TOKEN"


spawn sudo openconnect https://vpn-nl.sanoma.com/OTP


expect { 
  "\[sudo\]" {
      stty -echo
      send_user -- "Sudo Password: "
      expect_user -re "(.*)\n"
      send_user "\n"
      stty echo
      set pass $expect_out(1,string)
      send "$pass\r"
    }
    default {}
    
}

expect "Username" {
  send "media\\E.Klerks\r"
}

expect "Password" { 
  stty -echo 
  send_user -- "AD Password: "
  expect_user -re "(.*)\n"
  send_user "\n"
  stty echo 
  set pass $expect_out(1,string)
  send "$pass\r"
}

expect { 
  "POST" {} 
}

expect {
  "Login failed" {
    puts "\n"
    puts "Login failed for whatever reason"
    exit 1 
  }
  "Enter"
}



expect "Response" {
  send "$TOKEN\r"
}
expect "POST"

expect {
  "Login failed" {
    puts "\n"
    puts "Login failed for whatever reason"
    exit 1 

  }
  default { 
    interact
  }
}



