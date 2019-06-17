FROM typelead/eta

USER root



RUN apt-get update && apt-get install curl zlib1g-dev libncurses5-dev libbz2-dev -y && export LC_ALL=en_US.UTF-8 && export LANG=en_US.UTF-8 

RUN curl -sSL https://get.haskellstack.org/ | sh


