# Microservices Toys

# go-service

| Command             | Description                              |
|---------------------|------------------------------------------|
| make dep            | Installs and updates dependencies        |
| make run            | Runs the service locally                 |
| make build          | Builds the service binary locally        |
| make docker         | Builds Docker image                      |
| make up             | Runs the service locally in containers   |
| make down           | Stops and removes local containers       |
| make test           | Runs the unit tests                      |
| make coverage       | Runs the unit tests with coverage report |
| make test-component | Runs the component tests                 |

# node-service

| Command                 | Description                               |
|-------------------------|-------------------------------------------|
| yarn start              | Runs the service locally                  |
| yarn run nsp            | Identifies known vulneberities in service |
| yarn run lint           | Runs standard linter                      |
| yarn run test           | Runs the unit tests                       |
| yarn run test-component | Runs the component tests                  |
| make docker             | Builds Docker image                       |
| make up                 | Runs the service locally in containers    |
| make down               | Stops and removes local containers        |
| make docker-test        | Builds Docker test image                  |
| make test               | Runs the unit tests in containers         |
| make test-component     | Runs the component tests in containers    |
