FROM gitpod/workspace-full

USER root

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN apt-get update && apt-get install zlib1g-dev libncurses5-dev libbz2-dev && export LC_ALL=en_US.UTF-8 && export LANG=en_US.UTF-8

USER gitpod

RUN git clone https://github.com/haskell/haskell-ide-engine --recursive && \
    cd haskell-ide-engine && \
    stack install

RUN cd /workspace && mkdir bin && cd bin && \
    curl https://cdnverify.eta-lang.org/eta-binaries/etlas-1.5.0.0/binaries/x86_64-linux/etlas > etlas && \
    chmod 777 etlas && \
    PATH=/workspace/bin:$PATH

RUN cd /workspace/eta && ./install.sh

