#!/bin/sh
(cd examples/kazoo/chapter_8 && crystal spec) && (cd examples/brainfuck/ && crystal spec) && (cd examples/exp_lang && crystal spec) && (cd examples/json_parser/ && crystal spec) && crystal spec
