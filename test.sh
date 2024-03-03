#/usr/bin/bash
# run file in test folder using both GCC and ACC

if [ -z "$1" ]
  then
    echo "No argument supplied"
    exit -1
fi

input="./test/${1}.c"

if [ ! -f "${input}" ]; then
    echo "File not found: ${input}"
    exit -1
fi

echo "GCC ----------------------"
gcc.exe $input -o test/${1}_gcc.exe && ./test/${1}_gcc.exe; echo return code: $?
echo "--------------------------"
echo
echo "ACC ----------------------"
cargo.exe run -- build -k --ast $input && ./test/${1}.exe; echo return code: $?
echo "--------------------------"
