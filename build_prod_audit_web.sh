#!/bin/bash

# on netlify npm removes all other packages when installing bower...
# netlify sample: > added 1 package and removed 507 packages in 6.684s
yarn install

yarn audit-prod-web

