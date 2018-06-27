#!/bin/bash

OUTPUT_CACHE="./node_modules/__cache__output"

# on netlify npm removes all other packages when installing bower...
# netlify sample: > added 1 package and removed 507 packages in 6.684s
yarn install $YARN_FLAGS

# if [ -n "$BRANCH" ] && [ "$BRANCH" = "master" ]; then
#   bower cache clean
#   rm -rf bower_components
#   bower install
# fi

if [ -d "$OUTPUT_CACHE" ]; then
  echo "\n\n## COPYING ./output CACHE ##"
  cp -a "$OUTPUT_CACHE" ./output
  du -h ./output | tail -n 1
  echo -e "\n\n"
fi

npm run audit-prod-web

echo -e "\n\n## Caching ./output ##\n\n"
rm -rf "$OUTPUT_CACHE"
cp -a ./output "$OUTPUT_CACHE"
