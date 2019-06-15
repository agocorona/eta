FROM typelead/eta

USER root

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN apt-get update && apt-get install zlib1g-dev libncurses5-dev libbz2-dev -y && export LC_ALL=en_US.UTF-8 && export LANG=en_US.UTF-8 



