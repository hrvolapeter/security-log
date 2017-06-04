# Security log 🔒🔒🔒
[![Build Status](https://travis-ci.org/retep007/security-log.svg?branch=master)](https://travis-ci.org/retep007/security-log)
[![Coverage Status](https://coveralls.io/repos/github/retep007/security-log/badge.svg?branch=master)](https://coveralls.io/github/retep007/security-log?branch=master)

Security log is a high performence access log analyzer for OWASP TOP 10 attacks (well only part that can be detected from access logs 😉). This project was created as part of my bachelor thesis.

[Download](https://github.com/retep007/security-log/archive/latest.zip) precompiled binary for Ubuntu

## Contents
- [Features](#features)
- [Options](#options-%EF%B8%8F)
- [Building from source](#building-from-source)

## Features 
- analyse web access logs
- nginx / apache format
- reading from file or elasticsearch
- reporting using email ✉️
- running as a daemon 👻 

## Options ⌨️
Configurable using config stored in `/etc/security-log/config.yaml`

**Input types:** 🕸

* 🕵️ elasticsearch
* 📂 file path

**Output types / Incident reporting** #️⃣

* ➥ std
* ✉️ email

**Server type**

* nginx
* apache


Apache, elasticsearch with output to std
```markdown
tag: Config
serverType: Apache
input:
  tag: Elastic
  size: 10
  ip: http://localhost:9200
output:
  tag: Std
asDaemon: false
```

Nginx, file, with output to email runing as daemon
```markdown
tag: Config
serverType: Nginx
input:
  tag: File
  content: /logs/apache/acces_log
output:
  tag: Email
  content: test@me.com
asDaemon: true
```

## Building From Source
Install haskell [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) with ```bash
curl -sSL https://get.haskellstack.org/ | sh
```
Compile using ```bash
stack build
```