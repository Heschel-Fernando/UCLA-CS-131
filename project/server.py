import sys
import asyncio
import argparse
import time
import aiohttp
import json

# https://docs.aiohttp.org/en/stable/
class HttpClient:
	async def fetch(self, session, url):
		async with session.get(url) as response:
			return await response.json()

	async def get(self, url):
		async with aiohttp.ClientSession(connector=aiohttp.TCPConnector(ssl=False)) as session:
			return await self.fetch(session, url)

class ApiManager:

	def __init__(self):
		self.key = '...'
		self.base_url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json'
		self.httpClient = HttpClient()

	async def search_near_by(self, location, radius):
		return await self.httpClient.get('{}?key={}&location={}&radius={}'.format(self.base_url, self.key, location, radius))

class TypeChecker:

	def is_float(self, value):
		try:
			float(value)
			return True
		except ValueError:
			return False

	def is_int(self, value):
		try:
			int(value)
			return True
		except ValueError:
			return False


class FormatConverter:

	def message2list(self, message):
		return [msg for msg in message.strip().split() if len(msg)]

	def location2list(self, location):
		return location.replace("+", " + ").replace("-", " - ").split()

	def json2string(self, json_data):
	    '''
	    actually indent is optional, simply putting here to make it look better
	        so as below in save_json
	    '''
	    return json.dumps(json_data, indent=4)

class FormatChecker:

	def __init__(self):
		self.typeChecker = TypeChecker()
		self.formatConverter = FormatConverter()

	def is_iso_6709(self, value):
		location_list = self.formatConverter.location2list(value)
		if len(location_list) != 4:
			return False

		if not self.typeChecker.is_float(location_list[1]):
			return False

		if not self.typeChecker.is_float(location_list[3]):
			return False

		return True

	def is_posix_time(self, value):
		return self.typeChecker.is_float(value)


class Server:
	def __init__(self, name, ip='127.0.0.1', port=8888, message_max_length=1e6):

		# https://github.com/CS131-TA-team/CS131-Project-Sample-Grading-Script/blob/master/others/ports_assigned.csv
		self.port_map = {
			'Hill': 11825,
			'Jaquez': 11826,
			'Smith': 11827,
			'Campbell': 11828,
			'Singleton': 11829
		}

		'''
		Hill talks with Jaquez and Smith.
		Singleton talks with everyone else but Hill.
		Smith talks with Campbell.

		Note 'talks with' is bidirectional
		'''
		self.network_flow = {
			'Hill': ['Jaquez', 'Smith'],
			'Jaquez': ['Hill', 'Singleton'],
			'Smith': ['Hill', 'Campbell', 'Singleton'],
			'Campbell': ['Smith', 'Singleton'],
			'Singleton': ['Jaquez', 'Smith', 'Campbell']
		}

		# invalid server id
		if name not in self.port_map:
			self.print_invalid_server_name_message(name)
			sys.exit(1)

		self.name = name
		self.ip = ip
		self.port = self.port_map[name]
		self.message_max_length = int(message_max_length)

		self.apiManager = ApiManager()
		self.typeChecker = TypeChecker()
		self.formatChecker = FormatChecker()
		self.formatConverter = FormatConverter()

		'''
		client history
		client_id => message_list
		message_list[0] = 'AT'
		message_list[1] = server_id
		message_list[2] = time difference between client sending message and server receiving it
		message_list[3] = client_id
		message_list[4] = latitude and longitude in ISO 6709 notation
		message_list[5] = time of message sent in POSIX time (client)
		'''
		self.history = dict()

		self.log_file = open(self.name + '.log', 'w', 1)

	def print_invalid_server_name_message(self, invalid_server_name):
		print("error: invalid server_name \"{}\"".format(invalid_server_name))
		print("valid server names: {}".format(list(self.port_map.keys())))

	def calculate_time_diff(self, time_received_at_server, time_received_at_client):
		time_diff = time_received_at_server - time_received_at_client
		if time_diff > 0: 
			time_diff = '+{}'.format(time_diff)
		else: 
			time_diff = str(time_diff)
		return time_diff

	async def respond(self, writer, sendback_message):
		addr = writer.get_extra_info('peername')
		self.log_file.write("SEND to {}: {}\n".format(addr, sendback_message))

		writer.write(sendback_message.encode())
		await writer.drain()

		writer.close()

	async def handle_at_command(self, message_list):
		'''
		message_list[0] = 'AT'
		message_list[1] = server_id
		message_list[2] = time difference between client sending message and server receiving it
		message_list[3] = client_id
		message_list[4] = latitude and longitude in ISO 6709 notation
		message_list[5] = time of message sent in POSIX time (client)
		message_list[6] = server_id the message propagated from
		'''

		# already propagated or old propagation
		if message_list[3] in self.history and message_list[5] <= self.history[message_list[3]][5]: 
			return				
		
		# client's info must be added or updated
		self.history[message_list[3]] = message_list

		# propagate location updates
		message = " ".join(message_list)
		await asyncio.create_task(self.flood_to_all(message))


	async def handle_iamat_command(self, writer, message_list, time_received):
		'''
		message_list[0] = 'IAMAT'
		message_list[1] = client_id
		message_list[2] = latitude and longitude in ISO 6709 notation
		message_list[3] = time of message sent in POSIX time

		sendback_message_list[0] = 'AT'
		sendback_message_list[1] = server_id
		sendback_message_list[2] = (time of message received) - message_list[3]
		sendback_message_list[3] = message_list[1]
		sendback_message_list[4] = message_list[2]
		sendback_message_list[5] = message_list[3]
		'''

		sendback_message_list = []
		sendback_message_list.append('AT')
		sendback_message_list.append(self.name)
		sendback_message_list.append(self.calculate_time_diff(time_received, float(message_list[3])))
		sendback_message_list.append(message_list[1])
		sendback_message_list.append(message_list[2])
		sendback_message_list.append(message_list[3])

		self.history[message_list[1]] = sendback_message_list

		sendback_message = " ".join(sendback_message_list)

		await asyncio.gather(self.respond(writer, sendback_message), self.flood_to_all(sendback_message))


	async def handle_whatsat_command(self, writer, message_list, time_received):
		'''
		message_list[0] = 'WHATSAT'
		message_list[1] = client_id
		message_list[2] = radius from the client
		message_list[3] = upper bound on the amount of information to receive

		sendback_message_list[0] = 'AT'
		sendback_message_list[1] = server_id
		sendback_message_list[2] = time difference
		sendback_message_list[3] = message_list[1]
		sendback_message_list[4] = location (e.g. +34.068930-118.445127)
		sendback_message_list[5] = time
		(optional) sendback_message_list[6] = server_id the message propagated from
		'''
		sendback_message_list = self.history[message_list[1]][:6]
		
		location_list = self.formatConverter.location2list(sendback_message_list[4])
		location = '{}{},{}{}'.format(location_list[0], location_list[1], location_list[2], location_list[3])

		# km to m
		radius = float(message_list[2]) * 1000

		data = await self.apiManager.search_near_by(location, radius)
		data['results'] = data['results'][:int(message_list[3])]

		sendback_message = " ".join(sendback_message_list)
		sendback_message += '\n'
		sendback_message += self.formatConverter.json2string(data)
		sendback_message += '\n\n'

		await self.respond(writer, sendback_message)

	async def handle_invalid_command(self, writer, message_list):
		sendback_message = "? {}".format(" ".join(message_list))
		await self.respond(writer, sendback_message)


	def validate_message_list(self, message_list):
		
		if len(message_list) == 0:
			return False

		if message_list[0] == 'AT':
			'''
			message_list[0] = 'AT'
			message_list[1] = server_id
			message_list[2] = time difference between client sending message and server receiving it
			message_list[3] = client_id
			message_list[4] = latitude and longitude in ISO 6709 notation
			message_list[5] = time of message sent in POSIX time (client)
			message_list[6] = server_id the message propagated from
			'''
			if len(message_list) != 7:
				return False

			if message_list[1] not in self.port_map:
				return False

			if not (message_list[2].startswith('+') or message_list[2].startswith('-')):
				return False

			if not self.typeChecker.is_float(message_list[2][1:]):
				return False

			if not self.formatChecker.is_iso_6709(message_list[4]):
				return False

			if not self.formatChecker.is_posix_time(message_list[5]):
				return False

			if not message_list[6] in self.port_map:
				return False

			return True

		elif message_list[0] == 'IAMAT':
			'''
			message_list[0] = 'IAMAT'
			message_list[1] = client_id
			message_list[2] = latitude and longitude in ISO 6709 notation
			message_list[3] = time of message sent in POSIX time
			'''

			if len(message_list) != 4:
				return False

			if not self.formatChecker.is_iso_6709(message_list[2]):
				return False

			if not self.formatChecker.is_posix_time(message_list[3]):
				return False

			return True


		elif message_list[0] == 'WHATSAT':
			'''
			message_list[0] = 'WHATSAT'
			message_list[1] = client_id
			message_list[2] = radius from the client
			message_list[3] = upper bound on the amount of information to receive
			'''
			if len(message_list) != 4:
				return False

			# client does not exist
			if not message_list[1] in self.history:
				return False

			if not self.typeChecker.is_float(message_list[2]):
				return False

			radius = float(message_list[2])
			if radius < 0 or 50 < radius:
				return False

			if not self.typeChecker.is_int(message_list[3]):
				return False

			count = int(message_list[3])
			if count < 0 or 20 < count:
				return False

			return True


		return False


	async def handle_echo(self, reader, writer):
		'''
		on server side
		'''

		# wait until the server receives data...
		data = await reader.read(self.message_max_length)

		time_received = time.time()

		message = data.decode()

		addr = writer.get_extra_info('peername')
		self.log_file.write("RECEIVED from {}: {}\n".format(addr, message))

		message_list = self.formatConverter.message2list(message)

		valid = self.validate_message_list(message_list)

		if not valid:

			await self.handle_invalid_command(writer, message_list)

		else:

			if message_list[0] == 'AT':
				
				await self.handle_at_command(message_list)
				
			elif message_list[0] == 'IAMAT':
				
				await self.handle_iamat_command(writer, message_list, time_received)
				
			elif message_list[0] == 'WHATSAT':
				
				await self.handle_whatsat_command(writer, message_list, time_received)
			


	def run_until_interrupted(self):
		loop = asyncio.get_event_loop()
		coro = asyncio.start_server(self.handle_echo, self.ip, self.port, loop=loop)
		server = loop.run_until_complete(coro)

		# Serve requests until Ctrl+C is pressed
		self.log_file.write('SERVING {} on {}\n'.format(self.name, server.sockets[0].getsockname()))
		try:
		    loop.run_forever()
		except KeyboardInterrupt:
		    pass
		# Close the server
		server.close()
		loop.run_until_complete(server.wait_closed())
		loop.close()
		# close the log file
		self.log_file.close()

	
	async def flood_to_all(self, message):
		'''
		sendback_message_list[0] = 'AT'
		sendback_message_list[1] = server_id
		sendback_message_list[2] = time difference between client sending message and server receiving it
		sendback_message_list[3] = client_id
		sendback_message_list[4] = location
		sendback_message_list[5] = time sent from client
		message = " ".join(sendback_message_list)
		'''

		async def flood_to_one(destination_server_name):

			self.log_file.write('CONNECTIING to {}\n'.format(destination_server_name))

			try:
				_, writer = await asyncio.open_connection(
					self.ip, 
					self.port_map[destination_server_name], 
					loop=asyncio.get_event_loop()
				)

				message_list = self.formatConverter.message2list(message)
				# propagating first time
				if len(message_list) == 6:
					message_list.append(self.name)
				else:
					message_list[6] = self.name

				updated_message = " ".join(message_list)
				
				self.log_file.write('CONNECTED to {}\n'.format(destination_server_name))
				self.log_file.write('SEND to {}: {}\n'.format(destination_server_name, updated_message))
				writer.write(updated_message.encode())

				await writer.drain()

				self.log_file.write('DISCONNECTED from {}\n'.format(destination_server_name))
				writer.close()

			except ConnectionRefusedError:
				self.log_file.write('CONNECTION REFUSED from {}\n'.format(destination_server_name))

			except IndexError:
				self.log_file.write('LOGIC ERROR ON SERVER')

		# * expands a list to arguments in function call
		await asyncio.gather(*map(flood_to_one, self.network_flow[self.name]))

def main():

	parser = argparse.ArgumentParser()
	parser.add_argument('server_name', type=str, help='required server name input')
	args = parser.parse_args()

	server = Server(args.server_name)

	server.run_until_interrupted()


if __name__ == '__main__':
	main()


