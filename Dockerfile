FROM ubuntu:20.04

# Copy the current folder which contains C++ source code to the Docker image
COPY . /usr/src

# Avoid interactive dialog from tzdata
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y && apt-get install -y \
    cmake \
    make \
    python3 \
    python3-pip \
    ninja-build \
    g++-9

ENV CXX=g++-9
ENV CC=gcc-9
RUN pip3 install conan

# Specify the working directory
WORKDIR /usr/src
