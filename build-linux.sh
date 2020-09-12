#!/usr/bin/env bash

cmake . -Bbuild -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_UNITY_BUILD=ON -DENABLE_IPO=OFF -DTREAT_WARNINGS_AS_ERRORS=OFF
cmake --build build/
