FROM gitpod/workspace-full

USER root

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN apt-get update && apt-get install zlib1g-dev libncurses5-dev libbz2-dev && export LC_ALL=en_US.UTF-8 && export LANG=en_US.UTF-8

USER gitpod

RUN git clone https://github.com/haskell/haskell-ide-engine --recursive && \
    cd haskell-ide-engine && \
    stack install

