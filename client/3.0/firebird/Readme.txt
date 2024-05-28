The FirebirdOOAPI.pas unit defines an Object Oriented (OO) interface provided by the Firebird Client library. This interface is documented in the file "Using_OOAPI.html" include with the Firebird source code distribution.

FirebirdOOAPI.pas has been generated from the interface specification contained in FirebirdInterface.idl. This describes the API as a set of interfaces, not dissimilar to Pascal interfaces, and which comprises, for interface, a set of functions and procedures (methods); most interfaces also inherit from a super class.

A utility program "cloop" (https://github.com/asfernandes/cloop) is used to process the interface description and generate language bindings for both the provider and the user of the interfaces. For the Firebird client library, cloop generates a set of C++ bindings. It may also generate Pascal bindings for use with Delphi and Free Pascal.

Common to all language bindings is the idea that each call to an interface is made using a pointer to a memory block comprising a null pointer followed by a pointer to a method table (the vTable). a method table comprises a Null pointer followed by an integer version number and pointers to each of the interface's methods as defined by the interface description (idl file). This approach is intended to map on to a typical object data structure in a programming language. For example, in Delphi and Free Pascal, the underlying data structure for each object instance starts with a pointer to the class's virtual method table following by each of the variables in their declaration order.

For example, the Firebird Interface includes an interface called "IMaster". A pointer to the IMaster memory block is returned by the Firebird client library's exported entry point fb_get_master_interface. This memory block is provided by the client library's implementation of IMaster. The IMaster methods are typically concerned with access to other interfaces. IMaster.getUtilInterface returns a pointer to the IUtil interface memory block, and so on. When calling IMaster.getUtilInterface the caller provides the pointer to the IMaster memory block thus giving context to the call and allowing the provider to locate the object implementing the interface.

The original implementation of the Firebird OOAPI for Pascal implemented each interface as a Pascal class following an inheritance tree identical to that in the idl file, and comprising methods as listed in the idl file. Code was also generated for each method that called the actual method via the vTable. Given the data layout of each vTable, Pascal classes could also be used to describe the vTable layout.

The Pascal implementation of the Firebird OOAPI also allowed for an implementation of each interface as an abstract class. In simple uses of the OOAPI these could be ignored, except for version number callbacks and event callbacks. Only with user provided "udr libraries" would there be an significant use of the interface implementation classes.

Each "implementation class" was a subclass of the corresponding "interface class" but with each of the interface's methods redefined as an abstract method. A vTable was also created and assigned to an instance of the implementation class. The vTable methods called the classes corresponding abstract method. Thus, in order to provide an implementation of an interface, all the user would need to do was to subclass the abstract class, override each abstract method and provide a suitable implementation of each method. 

An instance of such an implementation was type compatible with the corresponding "interface" super class and hence could be provided as a parameter to an API call that required an instance of the class. For example, the IUtil interface includes the method:

procedure getFbVersion(status: IStatus; att: IAttachment; callback: IVersionCallback);

The OOAPI declares an abstract class 

IVersionCallbackImpl = class(IVersionCallback)
  procedure callback(status: IStatus; text: PAnsiChar); virtual; abstract;   
end;

The interface user may then subclass IVersionCallbackImpl with an implementation of the "callback" procedure (to receive the version text) and provide an instance of that class as the 3rd argument of the call to IUtil.getFbVersion. The reader may find an implemented example of this in the unit FB30Attachment.pas and TFB30Attachment.getFBVersion (updated for 2024 as described below).

2024 Update
===========

The Pascal OOAPI interface has had to be radically redesigned. This is because, in their infinite wisdom, the Free Pascal devs saw fit,with FPC 3.3.1 and later, to change the layout of an object instance. The change is to insert a new (system) field after the vmt pointer and before each of the object's variables. This is for use with the TMonitor class.  While similar functionality is also being added to Delphi, it is not clear whether the same approach has been used for implementation. 

The bottom line is that neither the vTable nor each interface can be described as a Pascal class. Instead, the only "safe" data structure is understood to be the Pascal record structure and this has been used for the 2024 update. For vTables, this approach is arguably better given that records can be initialised at compile time. The large initialization and finalization structures that were previously required, have now been removed. The unit should thus be smaller and faster to load.

However, the actual interfaces and now less elegant. They are also implemented as "records" with the extended record type allowing each such record to comprise the null pointer, vTable pointer and the interface's methods. However, there is no longer any means to inherit from one interface to another and hence each interface has to be fully defined with both its own and all inherited methods. Otherwise, the implementation of each method remains as before - a call via the vTable to the actual method.

The implementation classes remain as abstract classes but this time they form their own class hierarchy with no means to inherit from the "interface record". Implicit type compatibility has hence been lost and replaced by explicit type coercions. For example, IVersionCallbackImpl now includes:

function asIVersionCallback: IVersionCallback;

and any call to IUtil.getFbVersion now has to be revised so that the 3rd argument includes this explicit coercion.

Similarly, IVersionCallback now includes:

	  function isIVersionCallbackImpl: boolean;
	  function asIVersionCallbackImpl: IVersionCallbackImpl;

In udr libraries it is sometimes useful to check to see if an interface is implemented locally and to coerce it to the local implementation. The above two functions support this. The first is a test returning true or false, while the second is an explicit conversion (an exception is raised if the coercion fails). These two functions replace use of the "is" and "as" operators.

Given that the syntax for calling an extended record method is the same as that for an object instance method call, the updated language bindings have minimal disruption on most user code. The exceptions are:

1. "With" statements that reference an interface class (e.g. IMaster) need to include a pointer dereference ("^" appended to identifier).

2. when there is a need for type coercion, typically from an implementation object to the corresponding interface, this has now to use the explicit type cocercion methods.




