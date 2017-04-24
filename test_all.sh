#!/bin/sh
crystal spec &&
crystal spec examples/kazoo/chapter_8/spec &&
crystal spec examples/brainfuck/spec &&
crystal spec examples/exp_lang/spec &&
crystal spec examples/json_parser/spec
