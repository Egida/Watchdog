# Watchdog : AI-Powered DDOS Protection Engine
**Watchdog** is an AI-powered DDoS protection engine that analyzes your network packets and notifies you about suspicious addresses connected to your network.

We tested the **Watchdog** engine in one of the largest CDN providers in Iran, which handles 10% of the country’s traffic. Initially, the traffic was monitored by humans, but it was so inefficient that we had to create an AI-based solution. It took us about two years to make the project ready to launch. Now, we have decided to launch this project as a free DDoS protection Engine.

# Request
**Watchdog**  listens on **188.121.102.79:9001** & uses Transport Layer Security (TLS) to establish a secure connection.

## Packet Header Data Structure
|Field Size		|Description   |Comment  			  					        |
|---------------|--------------|------------------------------------------------|
| 2				| Length	   | Sequence + Checksum + Timestamp + Payload		|
| 4				| Sequnence    | Packet Sequence Number 			        	|
| 8				| Checksum     | First 8-bytes of SHA2-256 of **Payload**    	|
| 8				| Timestamp	   | Unix-Timestamp                                 |
| ?				| Payload	   |                                                |

> The maximum size of a packet is 65535 - 12. It’s worth noting that the **Sequence** header is non-functional and can be filled with random data.

## Payload Header Data Structure
|Field Size		|Description    |Comment  		    	                           |
|---------------|---------------|--------------------------------------------------|
| 2				| Layer	   		| Supports 7th Layer (0x0007) & 4th Layer (0x0004) |
| ?				| Packet Info   |                                                  |

## Packet Info Data Structure
|Field Size|Description     |
|----------|----------------|
| 4		   | Source IPv4    |
| 2	       | Source Port    |
| 4		   | Dest IPv4      |
| 2		   | Dest Port	    |
| 8		   | Unix-Timestamp |
| 2		   | Request Length |
| ?		   | Request	    | 

> **Request Length** & **Request** can get ignored while sending 4th layer requests. You can also append multiple **Packet Info** & send them together.

# Response
The **Packet Header** data structure is the same for both the request and the response. The response payload contains a 4-byte data that indicates a suspicious IPv4 address that is connected to your network. If the data structure is invalid, the connection will be closed. We are excited to announce that **Watchnet** will soon be available as an open-source project.

# WatchNet
**WatchNet** feeds data to **Watchdog**. It receives requests from servers and append each server's IPv4 address to it's requests, that's how **Watchdog** identifies each server. The data is stored for 4 to 5 days and then deleted, It is not used for any profitable purposes or shared with third parties.

> **WatchNet** is undergoing daily updates to improve its functionality. As a result, it may experience brief downtimes lasting a few seconds. While Erlang has built-in support for **Hot Code Swapping**, this feature has not been implemented in **WatchNet** at this time.

# Support

We greatly appreciate any help, feedback, or suggestions you can provide to improve the project. Your contributions can make a significant impact in enhancing the detection capabilities and overall effectiveness of **Watchdog**.

### How You Can Help

- **Testing and Feedback**: Deploy Watchdog in your network environment and provide feedback on its performance, accuracy, and usability. Report any bugs or issues you encounter, and share your suggestions for improvements.

- **Code Contributions**: If you have programming skills, you can contribute to the project by submitting code enhancements, bug fixes, or new features.

- **Documentation**: Help us improve the project's documentation by identifying areas that need clarification, providing examples, or suggesting additional content that would benefit users.

- **Spread the Word**: Share your positive experiences with Watchdog on social media, forums, or within your network security community. Your recommendations can help increase awareness and adoption of the project.

### Financial Contributions

Watchdog developed and maintained by a community of volunteers. We are passionate about providing a free and effective DDoS detection solution to the community. However, running and maintaining the project does incur costs for hosting, domain registration, and infrastructure.

If you find Watchdog valuable and would like to support the project financially, you can make a donation in Bitcoin to the following address:

```
bc1q49mkh7vjdfq0gxdv7s49vdnlhmvneqww2fve23
```

Your contributions will help cover the ongoing costs and enable us to allocate more resources to enhance the project's capabilities and provide better support to the community.

Please note that financial contributions are entirely optional, and we primarily value your active involvement, feedback, and contributions to the project.