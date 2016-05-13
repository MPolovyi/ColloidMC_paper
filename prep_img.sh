#!/bin/bash
(find . -type f -name "*.eps" -exec \cp -vuni '{}' "./Images" ";") #&> /dev/null
(find . -type f -name "*.png" -exec \cp -vuni '{}' "./Images" ";") #&> /dev/null
