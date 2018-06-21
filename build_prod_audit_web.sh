#!/bin/bash

# on netlify npm removes all other packages when installing bower...
# netlify sample: > added 1 package and removed 507 packages in 6.684s
yarn install

if [ -n "$BRANCH" ]; then 
  bower cache clean
  rm -rf bower_components
  bower install
fi

yarn audit-prod-web

