FROM typelead/eta

USER root

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN apt-get update && apt-get install zlib1g-dev libncurses5-dev libbz2-dev && export LC_ALL=en_US.UTF-8 && export LANG=en_US.UTF-8

RUN fallocate -l 3G /swapfile && chmod 600 /swapfile && mkswap /swapfile && swapon /swapfile

USER gitpod

