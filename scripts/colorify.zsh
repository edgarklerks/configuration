#!/usr/bin/zsh 
zmodload zsh/pcre  
        autoload -U colors 
        colors 

        lastcolor=${reset_color}
        while read line; do 
        if [[ ! -n $line ]]; then 
                continue 
        fi 

        pcre_compile -i "\[[^\]]+\]"
        if pcre_match -v date $line; then 

                
                # color error lines etc  
                pcre_compile -i "error"
                if pcre_match $line; then 
                        line="${fg_bold[red]}$line${lastcolor}"
                        lastcolor=${fg_bold[red]}
                        
                else  
                        pcre_compile -i "warning"
                        if pcre_match $line; then 
                                line="${fg_bold[yellow]}$line${lastcolor}"
                                lastcolor=${fg_bold[yellow]}
                        else 
#                                pcre_compile -i "(info|accepted|description|GET /|end connection)"
 #                               if pcre_match $line; then 
                                        line="${fg_bold[green]}$line${lastcolor}"
                                        lastcolor=${fg_bold[green]}
  #                              fi 
                        fi  
                fi 
                
                # highlight like fields  
                pcre_compile -i "\[[^\]]+\]"
                if pcre_match -v date $line; then 
                        line=${line//$date/${fg_bold[blue]}$date${lastcolor}}
                        lastcolor=${fg_bold[blue]}
                fi
                pcre_compile -i "([a-z]{3})"
                if pcre_match -a date $line; then 
                        for dat in $date; do 
#                                line=${line//$dat/${fg[blue]}$dat${lastcolor}}
#                                lastcolor=${fg[blue]}
                        done 
                fi 
        else 
                pcre_compile -i "SEVERE"
                if pcre_match -v date $line; then 
                        line="${fg_bold[red]}$line${lastcolor}"
                fi 
        fi 

        print "${lastcolor}${line}"
                
        done 
