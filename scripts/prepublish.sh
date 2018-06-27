#!/usr/bin/env bash

echo "### PREPUBLISH SCRIPT for sv-auditor ###"

npm run audit-prod
npm run audit-prod-web

cp ./_pureDist/audit-web.js ./packages/sv-auditor/dist/audit-web.js
cp ./_pureDist/audit-cli.js ./packages/sv-auditor/dist/audit-cli.js
