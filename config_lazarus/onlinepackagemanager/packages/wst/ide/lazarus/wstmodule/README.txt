
This is a fcl-web module to integrate WST in a fcl-web application.

The main class is the TWSTModule web module. It is registered in the Lazarus
IDE when the package is installed, and can be created using the File-New
module.

All that needs to be done is register the module in the correct location.

For instance using WST as the module name will make the module available
through the following URL:

http://myhost/WST

The default action is to display a list of defined service repositories.

The WSDL of a Service can be obtained through
http://myhost/WST/WSDL/ServiceName

To call a service, the URL is as follows:
http://myhost/WST/FORMAT/ServiceName

Where FORMAT is one of the supported encoding formats: SOAP, JSON, XML-RPC.

For example:
http://myhost/WST/SOAP/UserService


The following events exist:

BeforeGenerateWSDLTable,
AfterGenerateWSDLTable : Called before/after the list of services is generated.

BeforeGenerateWSDL,
AfterGenerateWSDL : Called before/after the WSDL for a specific service is
generated.

BeforeServiceRequest,
AfterServiceRequest: Called before/after a service request. In the
beforeservcierequest, a parameter can be set to indicate that the request
was handled by the event handler. Further processing of the request will
then be postponed

OnSetSessionProperties: Called when the request properties (using
IPropertyManager) are set. By default, the following properties are set:
SessionID : If CreateSession is 'True' the session ID is passed.
RemoteIP : The remote IP address (if available)
RemoteHost: The remote hostname (if available)
Additional properties can be written in a stringlist in the form
Name=Value.

Enjoy!

Michael.