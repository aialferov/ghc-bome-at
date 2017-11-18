FROM aialferov/alpinerl

ADD bin /bin
ADD etc /etc
ADD usr /usr

CMD /bin/ghc-bome-at \
    --config=/etc/ghc-bome-at.conf \
    --logdir=/var/log/ghc-bome-at \
    --suites=/usr/share/ghc-bome-at
