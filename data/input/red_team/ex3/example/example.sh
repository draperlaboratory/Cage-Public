#!/bin/bash
echo "Trying password: password123"
java -jar /home/challenge_program/pwcheck.jar password.txt password123
echo "Trying password: password"
java -jar /home/challenge_program/pwcheck.jar password.txt password
