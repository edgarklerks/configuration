#!/bin/bash

expect ~/scripts/vpn.tcl "$(ykman oath code Micro | awk '{print $2}')"                       
