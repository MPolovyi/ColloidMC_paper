#!/bin/bash
(find . -type f -name "*.eps" -exec cp -vuni '{}' "./Images" ";") >& /dev/null
