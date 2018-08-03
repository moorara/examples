# NATS Toys
This repo includes code examples demoing [NATS](https://nats.io) messaging system.

## pub-sub
This is an example of **publish-subscribe** pattern in which
all subscribers receive a message published on a subject.

```
make down
make pub-sub
```

## req-rep
This is an example of **request-reply** pattern in which
a requestor publishes a message on a subject and expects one or more reply messages.

```
make down
make req-rep
```

## queueing
This an example of **queuing** pattern in which
a messaged publsihed on a subject will be delivered to one **worker** in a group of workers.

```
make down
make queueing
```

## Documentation

  - https://www.nats.io/documentation
    - https://www.nats.io/documentation/concepts/nats-messaging
      - https://www.nats.io/documentation/concepts/nats-pub-sub
      - https://www.nats.io/documentation/concepts/nats-req-rep
      - https://www.nats.io/documentation/concepts/nats-queueing
    - https://www.nats.io/documentation/internals/nats-protocol-demo
      - https://www.nats.io/documentation/internals/nats-protocol
      - https://www.nats.io/documentation/internals/nats-server-protocol
    - https://www.nats.io/documentation/server/gnatsd-intro
      - https://www.nats.io/documentation/server/gnatsd-slow-consumers
    - https://www.nats.io/documentation/streaming/nats-streaming-intro
      - https://www.nats.io/documentation/streaming/nats-streaming-quickstart
      - https://www.nats.io/documentation/streaming/nats-streaming-protocol
    - https://github.com/nats-io/go-nats/tree/master/examples
      - https://www.nats.io/documentation/tutorials/nats-pub-sub
      - https://www.nats.io/documentation/tutorials/nats-req-rep
      - https://www.nats.io/documentation/tutorials/nats-queueing
      - https://www.nats.io/documentation/tutorials/nats-client-dev
      - https://www.nats.io/documentation/tutorials/nats-benchmarking
  - https://github.com/pires/kubernetes-nats-cluster
  - https://github.com/canhnt/k8s-nats-streaming
