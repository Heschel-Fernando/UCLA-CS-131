import asyncio
import argparse

class Client:
    def __init__(self, ip='127.0.0.1', port=11825, name='client', message_max_length=1e6):
        '''
        127.0.0.1 is the localhost
        port could be any port
        '''

        self.ip = ip
        self.port = port
        self.name = name
        self.message_max_length = int(message_max_length)

    async def tcp_echo_client(self, message, loop):
        '''
        on client side send the message for echo
        '''
        reader, writer = await asyncio.open_connection(self.ip, self.port,
                                                       loop=loop)
        print('{} send: {}'.format(self.name, message))
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print('{} received: {}'.format(self.name, data.decode()))

        print('close the socket')
        writer.close()

    def run_until_quit(self):
        # start the loop
        loop = asyncio.get_event_loop()
        while True:
            # collect the message to send
            message = input("Please input the next message to send: ")
            if message in ['quit', 'exit', ':q', 'exit;', 'quit;', 'exit()', '(exit)']:
                break
            else:
                loop.run_until_complete(self.tcp_echo_client(message, loop))
        loop.close()

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('server_name', type=str, help='required server name input')
    args = parser.parse_args()

    port_map = {
        'Hill': 11825,
        'Jaquez': 11826,
        'Smith': 11827,
        'Campbell': 11828,
        'Singleton': 11829
    }

    client = Client(port=port_map[args.server_name]) # using the default settings
    client.run_until_quit()



