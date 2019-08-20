#! /usr/bin/bash
# 1st: got GIT bash here
# 2nd: type:- bash myscript.sh or sh myscript.sh

# # ECHO command
# echo Hello World!

# # VARIABLES
# Note: Don't put whitespace below; i.e., NAME = "Brad" will throw up error
# NAME="Brad"
# echo "My name is ${NAME}"   # JS type strategy to write
# echo "My name is not $NAME"

# USER INPUT
# read -p "Mention name: " NAME
# echo "My name is $NAME"

# # IF-ELSE 
# NAME="Tom" 
# # Note the gaps. ["$NAME" == "Brad"] will show some issues
# if [ "$NAME" == "Brad" ]
# then
#     echo "Your name is Brad"
# elif [ "$NAME" == "Jack" ]
# then
#     echo "Your name is Jack"
# else
#     echo "Your name is neither Brad nor Jack"

# fi

# INEQUALITY COMPARERS

# List below:
# val1 -eq val2  : Returns True if they are equal
# val1 -ne val2  : Returns True if they are not equal
# val1 -gt val2  : Returns True if val1 > val2  , similarly lt
# val1 -ge val2  : Returns True if val1 >= val2 , similarly le

# N1=3    # Here also no space else shows command not found
# N2=6

# if [ "$N1" -gt "$N2" ]
# then
#     echo "$N1 is higher value"
# elif [ "$N1" -eq "$N2" ]
# then
#     echo "$N1 and $N2 are equal in value"
# else
#     echo "$N2 is higher value"
# fi

# # FILE CONDITIONS

# # List below:
# # -d file     True if file is a directory
# # -e file     True if file exists. Not used bcoz it's not portable
# # -f file     True if provided string is a file
# # -g file     True if group id is set on a file
# # -r file     True if file is readable
# # -s file     True if file has non-zero size
# # -u file     True if user id is set on a file
# # -w file     True if file is writable
# # -x file     True if file is executable

# FILE='Excel files'
# if [ -d "$FILE" ]
# then
#     echo "$FILE is an existing folder"
# else
#     echo "$FILE is not an existing folder"
# fi

# # Nested if-loops
# FILE2='3pk.c'
# if [ -f "$FILE2" ]
# then
#     echo "$FILE2 is an existing file"
#     if [ -r "$FILE2" ]
#     then
#         echo "$FILE2 is a readable file"
#     else
#         echo "$FILE2 is not an readable file"
#     fi

#     if [ -w "$FILE2" ]
#     then
#         echo "$FILE2 is a writeable file"
#     else
#         echo "$FILE2 is not an writeable file"
#     fi


# else
#     echo "$FILE2 is not an existing file"
# fi

# FILE3='3pk.exe'
# if [ -x "$FILE3" ]
# then
#     echo "$FILE3 is an executable file"
# else
#     echo "$FILE3 is not an executable file"
# fi

# # CASE STATEMENTS - to ask user of going ahead
# # Making a drink prompt
# read -p "You older than 21? " ANSWER
# case "$ANSWER" in 
#     [yY] | [yY][eE][sS])    # Notice there's only a closing bracket. Convention for arguments in case
#         echo "Drinking allowed!"
#         ;;                  # Closing of 1 of the case
#     [nN] | [nN][oO])
#         echo "Drinking NOT allowed!"
#         ;;
#     *)                      # Indicates that after this it goes to default action when none of the cases match!
#         echo "Please answer y/yes or n/no"
#         ;;
# esac

# FOR LOOP
# NAMES='Kevin Roger Brad Tom'
# for NAME in $NAMES
#     do 
#         echo "Hello, $NAME"
# done        

# # FOR LOOP to rename files
# FILES=$(ls *.txt)           # collect all txt files
# NEW='new'

# for FILE in $FILES
#     do 
#         echo "Renaming $FILE to $NEW-$FILE"
#         mv $FILE $NEW-$FILE
# done        

# # FUNCTION
# function sayHello(){
#     echo "Hello world"
# }

# sayHello

# # function with param
# function greet(){
#     echo "Hello $1, I am $2"
# }

# greet 'John' 'Kanishk'

# # CREATE a folder and write to it
# mkdir hello
# touch "hello/world.txt"
# echo "Hello world written in the txt file" >> "hello/world.txt"
# echo "Wrote the .txt file"

val1_array='1 2 3 4'
val2_array='11 12 13 14'

for VAL1 in $val1_array;
    do
        for VAL2 in $val2_array
            do 
                eval "python live_animation.py $VAL1 $VAL2"
        done
        
        
        # eval "python live_animation.py $VAL 3"
        # eval $command_line

done