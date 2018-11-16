#!/bin/bash

nome=$1

finger $nome | grep -i "Name: $nome" | cut -d' ' -f 2


