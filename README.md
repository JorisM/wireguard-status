# wireguard-status

a tool to see which clients are connected to your wireguard docker instance.
it does this by parsing the clients from the wg0.conf and then checking via the docker socket which ones are connected + how much data is being transmitted.

<img width="1123" alt="image" src="https://github.com/user-attachments/assets/969e2adc-f045-44b6-9d49-3eef2e5bf4c4">

# Use

you need to mount your `wg0.conf` and pass the docker socket (usually `/var/run/docker.sock`) of the instance where your wireguard runs.
once done, the only thing left is to set the name of the wireguard container to whatever you named yours with: `WG_CONTAINER_NAME`.

    wireguard-status:
        image: jorisdm/wireguard-status:main
        container_name: wireguard-status
        environment:
            - WG_CONF_FILE=/config/wg0.conf
            - WG_CONTAINER_NAME=wireguard
        restart: unless-stopped
        ports:
            - 8888:8888
        volumes:
            - ./vpn/config:/config
            - /var/run/docker.sock:/var/run/docker.sock

## Docker Compose

# Build

    stack build

# Build docker

    docker build .
