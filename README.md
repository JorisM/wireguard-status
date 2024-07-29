# wireguard-status

a tool to see which clients are connected to your wireguard docker instance.
it does this by parsing the clients from the wg0.conf and then checking via the docker socket which ones are connected + how much data is being transmitted.

![image](https://github.com/user-attachments/assets/7d3304cb-bf49-4f9a-97e8-7c7725f8e50c)


https://hub.docker.com/repository/docker/jorisdm/wireguard-status/general

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
