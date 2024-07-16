# wg-status

# Use

you need to mount your `wg0.conf` and pass the docker socket (usually `/var/run/docker.sock`) of the instance where your wireguard runs.
once done, the only thing left is to set the name of the wireguard container to whatever you named yours with: `WG_CONTAINER_NAME`.

    wireguard-status:
        build:
          context: ./wg-status
        container_name: wireguard-status
        environment:
          - WG_CONF_FILE=/wg0.conf
          - WG_CONTAINER_NAME=wireguard
        restart: unless-stopped
        ports:
            - 8888:8888
        volumes:
          - ./vpn/config/wg0.conf:/wg0.conf
          - /var/run/docker.sock:/var/run/docker.sock

## Docker Compose

# Build

    stack build

# Build docker

    docker build .
