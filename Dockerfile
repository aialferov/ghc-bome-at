FROM aialferov/alpinerl

ADD bin /bin

CMD /bin/ghc-bome-at \
    --config=/etc/ghc-bome-at.conf \
    --logdir=/var/log/ghc-bome-at \
    --suites=/usr/share/ghc-bome-at
