
(setq jquery-doc-hash (make-hash-table :size 500 :test (quote equal)))

(setq jquery-methods (quote nil))

(push "deferred.pipe" jquery-methods)

(puthash "deferred.pipe" (quote (("name" . "deferred.pipe") ("signatures" "deferred.pipe" (("doneFilter" "An optional function that is called when the Deferred is resolved.

" "true" nil) ("failFilter" "An optional function that is called when the Deferred is rejected.

" "true" nil))) ("desc" (text . "Utility method to filter and/or chain Deferreds.

")) ("longdesc" (text . "") (text . "The deferred.pipe() method returns a new promise that filters the
status and values of a deferred through a function. The doneFilter and
failFilter functions filter the original deferredʼs resolved / rejected
status and values. These filter functions can return a new value to be
passed along to the piped promiseʼs done() or fail() callbacks, or they
can return another observable object (Deferred, Promise, etc) which
will pass its resolved / rejected status and values to the piped
promiseʼs callbacks. If the filter function used is null, or not
specified, the piped promise will be resolved or rejected with the same
values as the original.
") (text . "")) ("examples" ((text . "") (text . "Filter resolve value:

") (text . "") (js . "
	var defer = $.Deferred(),
	filtered = defer.pipe(function( value ) {
	return value * 2;
	});
	
	defer.resolve( 5 );
	filtered.done(function( value ) {
	alert( \"Value is ( 2*5 = ) 10: \" + value );
	});
	") (text . "")) ((text . "") (text . "Filter reject value:

") (text . "") (js . "
	var defer = $.Deferred(),
	filtered = defer.pipe( null, function( value ) {
	return value * 3;
	});
	
	defer.reject( 6 );
	filtered.fail(function( value ) {
	alert( \"Value is ( 3*6 = ) 18: \" + value );
	});
	") (text . "")) ((text . "") (text . "Chain tasks:

") (text . "") (js . "
	var request = $.ajax( url, { dataType: \"json\" } ),
	chained = request.pipe(function( data ) {
	return $.ajax( url2, { data: { user: data.userId } } );
	});
	
	chained.done(function( data ) {
	// data retrieved from url2 as provided by the first request
	});
	
	") (text . ""))))) jquery-doc-hash)

(push "deferred.always" jquery-methods)

(puthash "deferred.always" (quote (("name" . "deferred.always") ("signatures" "deferred.always" (("alwaysCallbacks" "A function, or array of functions, that is called when the Deferred is
resolved or rejected.

" nil nil))) ("desc" (text . "Add handlers to be called when the Deferred object is either resolved
or rejected.

")) ("longdesc" (text . "") (text . "The argument can be either a single function or an array of functions.
When the Deferred is resolved or rejected, the alwaysCallbacks are
called. Since deferred.always() returns the Deferred object, other
methods of the Deferred object can be chained to this one, including
additional .always() methods. When the Deferred is resolved or
rejected, callbacks are executed in the order they were added, using
the arguments provided to the resolve , reject , resolveWith or
rejectWith method calls. For more information, see the documentation
for Deferred object.
") (text . "")) ("examples" ((text . "") (text . "Since the jQuery.get() method returns a jqXHR object, which is derived
from a Deferred object, we can attach a callback for both success and
error using the deferred.always() method.
") (text . "") (js . "
	$.get(\"test.php\").always( function() { 
	alert(\"$.get completed with success or error callback arguments\"); 
	} );
	") (text . ""))))) jquery-doc-hash)

(push "promise" jquery-methods)

(puthash "promise" (quote (("name" . "promise") ("signatures" "promise" (("type" "The type of queue that needs to be observed.

" "true" nil) ("target" "Object onto which the promise methods have to be attached

" "true" nil))) ("desc" (text . "Return a Promise object to observe when all actions of a certain type
bound to the collection, queued or not, have finished.

")) ("longdesc" (text . "") (text . "The .promise() method returns a dynamically generated Promise that is
resolved once all actions of a certain type bound to the collection,
queued or not, have ended.
") (text . "") (text . " By default, type is \"fx\", which means the returned Promise is resolved
when all animations of the selected elements have completed.

") (text . "") (text . " Resolve context and sole argument is the collection onto which
.promise() has been called.

") (text . "") (text . " If target is provided, .promise() will attach the methods onto it and
then return this object rather than create a new one. This can be
useful to attach the Promise behavior to an object that already exists.
") (text . "")) ("examples" ((text . "") (text . "Using promise on a collection with no active animation returns a
resolved Promise:

") (text . "") (js . "
	var div = $( \"<div />\" );
	
	div.promise().done(function( arg1 ) {
	// will fire right away and alert \"true\"
	alert( this === div && arg1 === div );
	});
	") (text . "")) ((text . "") (text . "Resolve the returned Promise when all animations have ended (including
those initiated in the animation callback or added later on):

") (text . "") (css . "
	div {
	height: 50px; width: 50px;
	float: left; margin-right: 10px;
	display: none; background-color: #090;
	}
	") (text . "") (html . "
	<button>Go</button>
	<p>Ready...</p>
	<div></div>
	<div></div>
	<div></div>
	<div></div>
	
	") (text . "") (js . "
	$(\"button\").bind( \"click\", function() {
	$(\"p\").append( \"Started...\");
  	
	$(\"div\").each(function( i ) {
	$( this ).fadeIn().fadeOut( 1000 * (i+1) );
	});
	
	$( \"div\" ).promise().done(function() {
	$( \"p\" ).append( \" Finished! \" );
	});
	});
	") (text . "")) ((text . "") (text . "Resolve the returned Promise using a $.when() statement (the .promise()
method makes it possible to do this with jQuery collections):

") (text . "") (css . "
	div {
	height: 50px; width: 50px;
	float: left; margin-right: 10px;
	display: none; background-color: #090;
	}
	") (text . "") (html . "
	<button>Go</button>
	<p>Ready...</p>
	<div></div>
	<div></div>
	<div></div>
	<div></div>
	
	") (text . "") (js . "
	var effect = function() {
	return $(\"div\").fadeIn(800).delay(1200).fadeOut();
	};
	
	$(\"button\").bind( \"click\", function() {
	$(\"p\").append( \" Started... \");
	
	$.when( effect() ).done(function() {
	$(\"p\").append(\" Finished! \");
	});
	});
	
	") (text . ""))))) jquery-doc-hash)

(push "removeProp" jquery-methods)

(puthash "removeProp" (quote (("name" . "removeProp") ("signatures" "removeProp" (("propertyName" "The name of the property to set.

" nil nil) ("value" "A value to set for the property.

" nil nil))) ("desc" (text . "Remove a property for the set of matched elements.

")) ("longdesc" (text . "The .removeProp() method removes properties set by the .prop() method.


") (text . "") (text . "With some built-in properties of a DOM element or window object,
browsers may generate an error if an attempt is made to remove the
property. jQuery first assigns the value undefined to the property and
ignores any error the browser generates. In general, it is only
necessary to remove custom properties that have been set on an object,
and not built-in (native) properties.
") (text . "") (text . "Note: Do not use this method to remove native properties such as
checked, disabled, or selected. This will remove the property
completely and, once removed, cannot be added again to element. Use
.prop() to set these properties to false instead.
") (text . "")) ("examples" ((text . "") (text . "Set a numeric property on a paragraph and then remove it.

") (text . "") (js . "
	var $para = $(\"p\");
	$para.prop(\"luggageCode\", 1234);
	$para.append(\"The secret luggage code is: \", String($para.prop(\"luggageCode\")), \". \");
	$para.removeProp(\"luggageCode\");
	$para.append(\"Now the secret luggage code is: \", String($para.prop(\"luggageCode\")), \". \");
	
	") (text . "") (css . "
	img { padding:10px; }
	div { color:red; font-size:24px; }
	") (text . "") (html . "
	<p></p>
	
	") (text . ""))))) jquery-doc-hash)

(push "prop" jquery-methods)

(puthash "prop" (quote (("name" . "prop") ("signatures" "prop" (("propertyName" "The name of the property to get.

" nil nil))) ("desc" (text . "Get the value of a property for the first element in the set of matched
elements.

")) ("longdesc" (text . "The .prop() method gets the property value for only the first element
in the matched set. It returns undefined for the value of a property
that has not been set, or if the matched set has no elements. To get
the value for each element individually, use a looping construct such
as jQueryʼs .each() or .map() method.
") (text . "") (text . "The difference between attributes and properties can be important in
specific situations. Before jQuery 1.6, the .attr() method sometimes
took property values into account when retrieving some attributes,
which could cause inconsistent behavior. As of jQuery 1.6, the .prop()
method provides a way to explicitly retrieve property values, while
.attr() only retrieves attributes.
") (text . "") (text . "For example, consider a DOM element defined by the HTML markup <input
type=\"checkbox\" checked=\"checked\" />, and assume it is in a JavaScript
variable named elem:
") (text . "") (text . "          elem.checked            true (Boolean)
     $(elem).prop(\"checked\")      true (Boolean)
  elem.getAttribute(\"checked\")    \"checked\" (String)
 $(elem).attr(\"checked\") (1.6+)   \"checked\" (String)
$(elem).attr(\"checked\") (pre-1.6) true (Boolean)
") (text . "") (text . "According to the W3C forms specification, the checked attribute is a
boolean attribute , which means the corresponding property is true if
the attribute is present at allâeven if, for example, the attribute has
no value or an empty string value. The preferred
cross-browser-compatible way to determine if a checkbox is checked is
to check for a \"truthy\" value on the elementʼs property using one of
the following:
") (text . "") (text . "  * if ( elem.checked )
  * if ( $(elem).prop(\"checked\") )
  * if ( $(elem).is(\":checked\") )
") (text . "") (text . "The code if ( $(elem).attr(\"checked\") ), on the other hand, will
retrieve the attribute, which does not change as the checkbox is
checked and unchecked. It is meant only to store the default or initial
value of the checked property.
") (text . "")) ("examples" ((text . "") (text . "Display the checked property and attribute of a checkbox as it changes.


") (text . "") (js . "
	$(\"input\").change(function() {
	var $input = $(this);
	$(\"p\").html(\".attr('checked'): <b>\" + $input.attr('checked') + \"</b><br>\"
	+ \".prop('checked'): <b>\" + $input.prop('checked') + \"</b><br>\"
	+ \".is(':checked'): <b>\" + $input.is(':checked') ) + \"</b>\";
	}).change();
	") (text . "") (css . "
	p { margin: 20px 0 0 }
	b { color: blue; }
	") (text . "") (html . "
	<input id=\"check1\" type=\"checkbox\" checked=\"checked\">
	<label for=\"check1\">Check me</label>
	<p></p>
	") (text . ""))))) jquery-doc-hash)

(push "prop" jquery-methods)

(puthash "prop" (quote (("name" . "prop") ("signatures" "prop" (("propertyName" "The name of the property to set.

" nil nil) ("value" "A value to set for the property.

" nil nil)) (("map" "A map of property-value pairs to set.

" nil nil)) (("propertyName" "The name of the property to set.

" nil nil) ("function(index, oldPropertyValue)" "A function returning the value to set. Receives the index position of
the element in the set and the old property value as arguments. Within
the function, the keyword this refers to the current element.
" nil nil))) ("desc" (text . "Set one or more properties for the set of matched elements.

")) ("longdesc" (text . "The .prop() method is a convenient way to set the value of
propertiesâespecially when setting multiple properties or using values
returned by a function. Properties generally affect the dynamic state
of a DOM element without changing the serialized HTML attribute.
Examples include the value property of input elements, the disabled
property of inputs and buttons, or the checked property of a checkbox.
Most often, .prop() should be used to set disabled and checked instead
of the .attr() method. The .val() method should be used for getting and
setting value.
") (text . "") (js . "
	  $(\"input\").prop(\"disabled\", false);
	  $(\"input\").prop(\"checked\", true);
	  $(\"input\").val(\"someValue\");
	") (text . "") (text . "Also note that the .removeProp() method should not be used to set these
properties to false. Once a native property is removed, it cannot be
added again. See .removeProp() for more information.
") (text . "")) ("examples" ((text . "") (text . "Disable all checkboxes on the page.

") (text . "") (js . "
	  $(\"input[type='checkbox']\").prop({
	  disabled: true
	  });
	  ") (text . "") (css . "
	  img { padding:10px; }
	  div { color:red; font-size:24px; }
	  ") (text . "") (html . "
	  <input type=\"checkbox\" checked=\"checked\" />
	  <input type=\"checkbox\" />
	  <input type=\"checkbox\" />
	  <input type=\"checkbox\"  checked=\"checked\" />
	  
	  ") (text . ""))))) jquery-doc-hash)

(push "$.ajaxPrefilter" jquery-methods)

(puthash "$.ajaxPrefilter" (quote (("name" . "$.ajaxPrefilter") ("signatures" "$.ajaxPrefilter" (("dataTypes" "An optional string containing one or more space-separated dataTypes

" "true" nil) ("handler(options, originalOptions, jqXHR)" "A handler to set default values for future Ajax requests.

" nil nil))) ("desc" (text . "Handle custom Ajax options or modify existing options before each
request is sent and before they are processed by $.ajax().

")) ("longdesc" (text . "") (text . "A typical prefilter registration using $.ajaxPrefilter() looks like
this:

") (text . "") (js . "
	    $.ajaxPrefilter( function( options, originalOptions, jqXHR ) {
	    // Modify options, control originalOptions, store jqXHR, etc
	    });
	  ") (text . "") (text . "where:


") (text . "") (text . "  * options are the request options
  * originalOptions are the options as provided to the ajax method,
    unmodified and, thus, without defaults from ajaxSettings
  * jqXHR is the jqXHR object of the request
") (text . "") (text . "Prefilters are a perfect fit when custom options need to be handled.
Given the following code, for example, a call to $.ajax() would
automatically abort a request to the same URL if the custom
abortOnRetry option is set to true:
") (text . "") (js . "
	    var currentRequests = {};
	    
	    $.ajaxPrefilter(function( options, originalOptions, jqXHR ) {
	    if ( options.abortOnRetry ) {
	    if ( currentRequests[ options.url ] ) {
	    currentRequests[ options.url ].abort();
	    }
	    currentRequests[ options.url ] = jqXHR;
	    }
	    });
	  ") (text . "") (text . "Prefilters can also be used to modify existing options. For example,
the following proxies cross-domain requests through
http://mydomain.net/proxy/:
") (text . "") (js . "
	    $.ajaxPrefilter( function( options ) {
	    if ( options.crossDomain ) {
	    options.url = \"http://mydomain.net/proxy/\" + encodeURIComponent( options.url );
	    options.crossDomain = false;
	    }
	    });
	  ") (text . "") (text . "If the optional dataTypes argument is supplied, the prefilter will be
only be applied to requests with the indicated dataTypes. For example,
the following only applies the given prefilter to JSON and script
requests:
") (text . "") (js . "
	    $.ajaxPrefilter( \"json script\", function( options, originalOptions, jqXHR ) {
	    // Modify options, control originalOptions, store jqXHR, etc
	    });
	  ") (text . "") (text . "The $.ajaxPrefilter() method can also redirect a request to another
dataType by returning that dataType. For example, the following sets a
request as \"script\" if the URL has some specific properties defined in
a custom isActuallyScript() function:
") (text . "") (js . "
	    $.ajaxPrefilter(function( options ) {
	    if ( isActuallyScript( options.url ) ) {
	    return \"script\";
	    }
	    });
	  ") (text . "") (text . "This would ensure not only that the request is considered \"script\" but
also that all the prefilters specifically attached to the script
dataType would be applied to it.
") (text . "")) ("examples"))) jquery-doc-hash)

(push "$.holdReady" jquery-methods)

(puthash "$.holdReady" (quote (("name" . "$.holdReady") ("signatures" "$.holdReady" (("hold" "Indicates whether the ready hold is being requested or released

" nil nil))) ("desc" (text . "Holds or releases the execution of jQueryʼs ready event.

")) ("longdesc" (text . "") (text . "The $.holdReady() method allows the caller to delay jQueryʼs ready
event. This advanced feature would typically be used by dynamic script
loaders that want to load additional JavaScript such as jQuery plugins
before allowing the ready event to occur, even though the DOM may be
ready. This method must be called early in the document, such as in the
<head> immediately after the jQuery script tag. Calling this method
after the ready event has already fired will have no effect.
") (text . "") (text . "To delay the ready event, first call $.holdReady(true). When the ready
event should be released to execute, call $.holdReady(false). Note that
multiple holds can be put on the ready event, one for each
$.holdReady(true) call. The ready event will not actually fire until
all holds have been released with a corresponding $.holdReady(false)
and the normal document ready conditions are met. (See ready for more
information.)
") (text . "")) ("examples" ((text . "") (text . "Delay the ready event until a custom plugin has loaded.

") (text . "") (js . "
	  $.holdReady(true);
	  $.getScript(\"myplugin.js\", function() {
	  $.holdReady(false);
	  });
	  ") (text . ""))))) jquery-doc-hash)

(push "$.hasData" jquery-methods)

(puthash "$.hasData" (quote (("name" . "$.hasData") ("signatures" "$.hasData" (("element" "A DOM element to be checked for data.

" nil nil))) ("desc" (text . "Determine whether an element has any jQuery data associated with it.

")) ("longdesc" (text . "The jQuery.hasData() method provides a way to determine if an element
currently has any values that were set using jQuery.data() . If no data
is associated with an element (there is no data object at all or the
data object is empty), the method returns false; otherwise it returns
true.
") (text . "") (text . "The primary advantage of jQuery.hasData(element) is that it does not
create and associate a data object with the element if none currently
exists. In contrast, jQuery.data(element) always returns a data object
to the caller, creating one if no data object previously existed.
") (text . "")) ("examples" ((text . "") (text . "Set data on an element and see the results of hasData.

") (text . "") (js . "
	  $(function(){
	  var $p = jQuery(\"p\"), p = $p[0];
	  $p.append(jQuery.hasData(p)+\" \"); /* false */
	  jQuery.data(p, \"testing\", 123);
	  $p.append(jQuery.hasData(p)+\" \"); /* true*/
	  jQuery.removeData(p, \"testing\");
	  $p.append(jQuery.hasData(p)+\" \"); /* false */
	  });
	  ") (text . "") (html . "<p>Results: </p>") (text . ""))))) jquery-doc-hash)

(push "$.now" jquery-methods)

(puthash "$.now" (quote (("name" . "$.now") ("signatures" "$.now" nil) ("desc" (text . "Return a number representing the current time.

")) ("longdesc" (text . "") (text . "The $.now() method is a shorthand for the number returned by the
expression (new Date).getTime().

") (text . "")) ("examples"))) jquery-doc-hash)

(push "deferred.promise" jquery-methods)

(puthash "deferred.promise" (quote (("name" . "deferred.promise") ("signatures" "deferred.promise" (("target" "Object onto which the promise methods have to be attached

" "true" nil))) ("desc" (text . "Return a Deferredʼs Promise object.

")) ("longdesc" (text . "") (text . "The deferred.promise() method allows an asynchronous function to
prevent other code from interfering with the progress or status of its
internal request. The Promise exposes only the Deferred methods needed
to attach additional handlers or determine the state ( then, done,
fail, isResolved, and isRejected), but not ones that change the state (
resolve, reject, resolveWith, and rejectWith). As of jQuery 1.6, the
Promise also exposes the always and pipe Deferred methods.
") (text . "") (text . "If target is provided, deferred.promise() will attach the methods onto
it and then return this object rather than create a new one. This can
be useful to attach the Promise behavior to an object that already
exists.
") (text . "") (text . "If you are creating a Deferred, keep a reference to the Deferred so
that it can be resolved or rejected at some point. Return only the
Promise object via deferred.promise() so other code can register
callbacks or inspect the current state.
") (text . "") (text . "For more information, see the documentation for Deferred object.


") (text . "")) ("examples" ((text . "") (text . "Create a Deferred and set two timer-based functions to either resolve
or reject the Deferred after a random interval. Whichever one fires
first \"wins\" and will call one of the callbacks. The second timeout has
no effect since the Deferred is already complete (in a resolved or
rejected state) from the first timeout action.
") (text . "") (js . "// Create a Deferred and return its Promise
	  function asyncEvent(){
	  var dfd = new jQuery.Deferred();
	  setTimeout(function(){
	  dfd.resolve(\"hurray\");
	  }, Math.floor(Math.random()*1500));
	  setTimeout(function(){
	  dfd.reject(\"sorry\");
	  }, Math.floor(Math.random()*1500));
	  return dfd.promise();
	  }
	  
	  // Attach a done and fail handler for the asyncEvent
	  $.when( asyncEvent() ).then(
	  function(status){
	  alert( status+', things are going well' );
	  },
	  function(status){
	  alert( status+', you fail this time' );
	  }
	  );
	  ") (text . "")) ((text . "") (text . "Use the target argument to promote an existing object to a Promise:

") (text . "") (js . "
	  // Existing object
	  var obj = {
	  hello: function( name ) {
	  alert( \"Hello \" + name );
	  }
	  },
	  // Create a Deferred
	  defer = $.Deferred();
	  
	  // Set object as a promise
	  defer.promise( obj );
	  
	  // Resolve the deferred
	  defer.resolve( \"John\" );
	  
	  // Use the object as a Promise
	  obj.done(function( name ) {
	  this.hello( name ); // will alert \"Hello John\"
	  }).hello( \"Karl\" ); // will alert \"Hello Karl\"
	  ") (text . ""))))) jquery-doc-hash)

(push "$.parseXML" jquery-methods)

(puthash "$.parseXML" (quote (("name" . "$.parseXML") ("signatures" "$.parseXML" (("data" "a well-formed XML string to be parsed

" nil nil))) ("desc" (text . "Parses a string into an XML document.

")) ("longdesc" (text . "") (text . "jQuery.parseXML uses the native parsing function of the browser to
create a valid XML Document. This document can then be passed to jQuery
to create a typical jQuery object that can be traversed and
manipulated.
") (text . "")) ("examples" ((text . "") (text . "Create a jQuery object using an XML string and obtain the value of the
title node.

") (text . "") (html . "
	  <p id=\"someElement\"></p>
	  <p id=\"anotherElement\"></p>
	  
	  
	  ") (text . "") (js . "
	  var xml = \"<rss version='2.0'><channel><title>RSS Title</title></channel></rss>\",
	  xmlDoc = $.parseXML( xml ),
	  $xml = $( xmlDoc ),
	  $title = $xml.find( \"title\" );

	  /* append \"RSS Title\" to #someElement */
	  $( \"#someElement\" ).append( $title.text() );

	  /* change the title to \"XML Title\" */
	  $title.text( \"XML Title\" );

	  /* append \"XML Title\" to #anotherElement */
	  $( \"#anotherElement\" ).append( $title.text() );
	  ") (text . ""))))) jquery-doc-hash)

(push "$.when" jquery-methods)

(puthash "$.when" (quote (("name" . "$.when") ("signatures" "$.when" (("deferreds" "One or more Deferred objects, or plain JavaScript objects.

" nil nil))) ("desc" (text . "Provides a way to execute callback functions based on one or more
objects, usually Deferred objects that represent asynchronous events.

")) ("longdesc" (text . "") (text . "If a single Deferred is passed to jQuery.when, its Promise object (a
subset of the Deferred methods) is returned by the method. Additional
methods of the Promise object can be called to attach callbacks, such
as deferred.then . When the Deferred is resolved or rejected, usually
by the code that created the Deferred originally, the appropriate
callbacks will be called. For example, the jqXHR object returned by
jQuery.ajax is a Deferred and can be used this way:
") (js . "$.when( $.ajax(\"test.aspx\") ).then(function(ajaxArgs){ 
	  alert(ajaxArgs[1]); /* ajaxArgs is [ \"success\", statusText, jqXHR ] */
	  });") (text . "") (text . "If a single argument is passed to jQuery.when and it is not a Deferred,
it will be treated as a resolved Deferred and any doneCallbacks
attached will be executed immediately. The doneCallbacks are passed the
original argument. In this case any failCallbacks you might set are
never called since the Deferred is never rejected. For example:
") (js . "$.when( { testing: 123 } ).done(
	  function(x){ alert(x.testing); } /* alerts \"123\" */
	  );") (text . "") (text . "In the case where multiple Deferred objects are passed to jQuery.when,
the method returns the Promise from a new \"master\" Deferred object that
tracks the aggregate state of all the Deferreds it has been passed. The
method will resolve its master Deferred as soon as all the Deferreds
resolve, or reject the master Deferred as soon as one of the Deferreds
is rejected. If the master Deferred is resolved, it is passed the
resolved values of all the Deferreds that were passed to jQuery.when.
For example, when the Deferreds are jQuery.ajax() requests, the
arguments will be the jqXHR objects for the requests, in the order they
were given in the argument list.
") (text . "") (text . "In the multiple-Deferreds case where one of the Deferreds is rejected,
jQuery.when immediately fires the failCallbacks for its master
Deferred. Note that some of the Deferreds may still be unresolved at
that point. If you need to perform additional processing for this case,
such as canceling any unfinished ajax requests, you can keep references
to the underlying jqXHR objects in a closure and inspect/cancel them in
the failCallback.
") (text . "")) ("examples" ((text . "") (text . "Execute a function after two ajax requests are successful. (See the
jQuery.ajax() documentation for a complete description of success and
error cases for an ajax request).
") (text . "") (js . "$.when($.ajax(\"/page1.php\"), $.ajax(\"/page2.php\")).done(function(a1,  a2){
	  /* a1 and a2 are arguments resolved for the 
	  page1 and page2 ajax requests, respectively */
	  var jqXHR = a1[2]; /* arguments are [ \"success\", statusText, jqXHR ] */
	  if ( /Whip It/.test(jqXHR.responseText) ) {
	  alert(\"First page has 'Whip It' somewhere.\");
	  }
	  });
	  ") (text . "")) ((text . "") (text . "Execute the function myFunc when both ajax requests are successful, or
myFailure if either one has an error.

") (text . "") (js . "$.when($.ajax(\"/page1.php\"), $.ajax(\"/page2.php\"))
	  .then(myFunc, myFailure);
	  ") (text . ""))))) jquery-doc-hash)

(push "deferred.resolveWith" jquery-methods)

(puthash "deferred.resolveWith" (quote (("name" . "deferred.resolveWith") ("signatures" "deferred.resolveWith" (("context" "Context passed to the doneCallbacks as the this object.

" nil nil) ("args" "An optional array of arguments that are passed to the doneCallbacks.

" "true" nil))) ("desc" (text . "Resolve a Deferred object and call any doneCallbacks with the given
context and args.

")) ("longdesc" (text . "") (text . "Normally, only the creator of a Deferred should call this method; you
can prevent other code from changing the Deferredʼs state by returning
a restricted Promise object through deferred.promise() .
") (text . "") (text . "When the Deferred is resolved, any doneCallbacks added by deferred.then
or deferred.done are called. Callbacks are executed in the order they
were added. Each callback is passed the args from the .resolve(). Any
doneCallbacks added after the Deferred enters the resolved state are
executed immediately when they are added, using the arguments that were
passed to the .resolve() call. For more information, see the
documentation for Deferred object.
") (text . "")) ("examples"))) jquery-doc-hash)

(push "deferred.rejectWith" jquery-methods)

(puthash "deferred.rejectWith" (quote (("name" . "deferred.rejectWith") ("signatures" "deferred.rejectWith" (("context" "Context passed to the failCallbacks as the this object.

" nil nil) ("args" "An optional array of arguments that are passed to the failCallbacks.

" "true" nil))) ("desc" (text . "Reject a Deferred object and call any failCallbacks with the given
context and args.

")) ("longdesc" (text . "") (text . "Normally, only the creator of a Deferred should call this method; you
can prevent other code from changing the Deferredʼs state by returning
a restricted Promise object through deferred.promise() .
") (text . "") (text . "When the Deferred is rejected, any failCallbacks added by deferred.then
or deferred.fail are called. Callbacks are executed in the order they
were added. Each callback is passed the args from the deferred.reject()
call. Any failCallbacks added after the Deferred enters the rejected
state are executed immediately when they are added, using the arguments
that were passed to the .reject() call. For more information, see the
documentation for Deferred object.
") (text . "")) ("examples"))) jquery-doc-hash)

(push "deferred.fail" jquery-methods)

(puthash "deferred.fail" (quote (("name" . "deferred.fail") ("signatures" "deferred.fail" (("failCallbacks" "A function, or array of functions, that are called when the Deferred is
rejected.

" nil nil) ("failCallbacks" "Optional additional functions, or arrays of functions, that are called
when the Deferred is rejected.

" "true" nil))) ("desc" (text . "Add handlers to be called when the Deferred object is rejected.

")) ("longdesc" (text . "") (text . "The deferred.fail() method accepts one or more arguments, all of which
can be either a single function or an array of functions. When the
Deferred is rejected, the failCallbacks are called. Callbacks are
executed in the order they were added. Since deferred.fail() returns
the deferred object, other methods of the deferred object can be
chained to this one, including additional deferred.fail() methods. The
failCallbacks are executed using the arguments provided to the
deferred.reject() or deferred.rejectWith() method call in the order
they were added. For more information, see the documentation for
Deferred object.
") (text . "")) ("examples" ((text . "") (text . "Since the jQuery.get method returns a jqXHR object, which is derived
from a Deferred, you can attach a success and failure callback using
the deferred.done() and deferred.fail() methods.
") (text . "") (js . "
	  $.get(\"test.php\")
	  .done(function(){ alert(\"$.get succeeded\"); })
	  .fail(function(){ alert(\"$.get failed!\"); });
	  ") (text . ""))))) jquery-doc-hash)

(push "deferred.done" jquery-methods)

(puthash "deferred.done" (quote (("name" . "deferred.done") ("signatures" "deferred.done" (("doneCallbacks" "A function, or array of functions, that are called when the Deferred is
resolved.

" nil nil) ("doneCallbacks" "Optional additional functions, or arrays of functions, that are called
when the Deferred is resolved.

" "true" nil))) ("desc" (text . "Add handlers to be called when the Deferred object is resolved.

")) ("longdesc" (text . "") (text . "The deferred.done() method accepts one or more arguments, all of which
can be either a single function or an array of functions. When the
Deferred is resolved, the doneCallbacks are called. Callbacks are
executed in the order they were added. Since deferred.done() returns
the deferred object, other methods of the deferred object can be
chained to this one, including additional .done() methods. When the
Deferred is resolved, doneCallbacks are executed using the arguments
provided to the resolve or resolveWith method call in the order they
were added. For more information, see the documentation for Deferred
object.
") (text . "")) ("examples" ((text . "") (text . "Since the jQuery.get method returns a jqXHR object, which is derived
from a Deferred object, we can attach a success callback using the
.done() method.
") (text . "") (js . "
	  $.get(\"test.php\").done(function() { 
	  alert(\"$.get succeeded\"); 
	  });
	  ") (text . "")) ((text . "") (text . "Resolve a Deferred object when the user clicks a button, triggering a
number of callback functions:

") (text . "") (js . "
	  // 3 functions to call when the Deferred object is resolved
	  function fn1() {
	  $(\"p\").append(\" 1 \");
	  }
	  function fn2() {
	  $(\"p\").append(\" 2 \");
	  }
	  function fn3(n) {
	  $(\"p\").append(n + \" 3 \" + n);
	  }
	  
	  // create a deferred object
	  var dfd = $.Deferred();
	  
	  // add handlers to be called when dfd is resolved
	  dfd
	  // .done() can take any number of functions or arrays of functions
	  .done( [fn1, fn2], fn3, [fn2, fn1] )
	  // we can chain done methods, too
	  .done(function(n) {
	  $(\"p\").append(n + \" we're done.\");
	  });
	  
	  // resolve the Deferred object when the button is clicked
	  $(\"button\").bind(\"click\", function() {
	  dfd.resolve(\"and\");
	  });
	  ") (text . "") (html . "
	  <button>Go</button>
	  <p>Ready...</p>
	  ") (text . ""))))) jquery-doc-hash)

(push "deferred.then" jquery-methods)

(puthash "deferred.then" (quote (("name" . "deferred.then") ("signatures" "deferred.then" (("doneCallbacks" "A function, or array of functions, called when the Deferred is
resolved.

" nil nil) ("failCallbacks" "A function, or array of functions, called when the Deferred is
rejected.

" nil nil))) ("desc" (text . "Add handlers to be called when the Deferred object is resolved or
rejected.

")) ("longdesc" (text . "") (text . "Both arguments can be either a single function or an array of
functions. Either argument can also be null if no callback of that type
is desired. Alternatively, use .done() or .fail() to set only
doneCallbacks or failCallbacks. When the Deferred is resolved, the
doneCallbacks are called. If the Deferred is instead rejected, the
failCallbacks are called. Callbacks are executed in the order they were
added. Since deferred.then returns the deferred object, other methods
of the deferred object can be chained to this one, including additional
.then() methods. For more information, see the documentation for
Deferred object.
") (text . "")) ("examples" ((text . "") (text . "Since the jQuery.get method returns a jqXHR object, which is derived
from a Deferred object, we can attach handlers using the .then method.

") (text . "") (js . "
	  $.get(\"test.php\").then(
	  function(){ alert(\"$.get succeeded\"); },
	  function(){ alert(\"$.get failed!\"); }
	  );
	  ") (text . ""))))) jquery-doc-hash)

(push "deferred.reject" jquery-methods)

(puthash "deferred.reject" (quote (("name" . "deferred.reject") ("signatures" "deferred.reject" (("args" "Optional arguments that are passed to the failCallbacks.

" nil nil))) ("desc" (text . "Reject a Deferred object and call any failCallbacks with the given
args.

")) ("longdesc" (text . "") (text . "Normally, only the creator of a Deferred should call this method; you
can prevent other code from changing the Deferredʼs state by returning
a restricted Promise object through deferred.promise() .
") (text . "") (text . "When the Deferred is rejected, any failCallbacks added by deferred.then
or deferred.fail are called. Callbacks are executed in the order they
were added. Each callback is passed the args from the deferred.reject()
call. Any failCallbacks added after the Deferred enters the rejected
state are executed immediately when they are added, using the arguments
that were passed to the .reject() call. For more information, see the
documentation for Deferred object.
") (text . "")) ("examples"))) jquery-doc-hash)

(push "deferred.isRejected" jquery-methods)

(puthash "deferred.isRejected" (quote (("name" . "deferred.isRejected") ("signatures" "deferred.isRejected" nil) ("desc" (text . "Determine whether a Deferred object has been rejected.

")) ("longdesc" (text . "") (text . "Returns true if the Deferred object is in the rejected state, meaning
that either deferred.reject() or deferred.rejectWith() has been called
for the object and the failCallbacks have been called (or are in the
process of being called).
") (text . "") (text . "Note that a Deferred object can be in one of three states: unresolved,
resolved, or rejected; use deferred.isResolved() to determine whether
the Deferred object is in the resolved state. These methods are
primarily useful for debugging, for example to determine whether a
Deferred has already been resolved even though you are inside code that
intended to reject it.
") (text . "")) ("examples"))) jquery-doc-hash)

(push "deferred.isResolved" jquery-methods)

(puthash "deferred.isResolved" (quote (("name" . "deferred.isResolved") ("signatures" "deferred.isResolved" nil) ("desc" (text . "Determine whether a Deferred object has been resolved.

")) ("longdesc" (text . "") (text . "Returns true if the Deferred object is in the resolved state, meaning
that either deferred.resolve() or deferred.resolveWith() has been
called for the object and the doneCallbacks have been called (or are in
the process of being called).
") (text . "") (text . "Note that a Deferred object can be in one of three states: unresolved,
resolved, or rejected; use deferred.isRejected() to determine whether
the Deferred object is in the rejected state. These methods are
primarily useful for debugging, for example to determine whether a
Deferred has already been resolved even though you are inside code that
intended to reject it.
") (text . "")) ("examples"))) jquery-doc-hash)

(push "deferred.resolve" jquery-methods)

(puthash "deferred.resolve" (quote (("name" . "deferred.resolve") ("signatures" "deferred.resolve" (("args" "Optional arguments that are passed to the doneCallbacks.

" nil nil))) ("desc" (text . "Resolve a Deferred object and call any doneCallbacks with the given
args.

")) ("longdesc" (text . "") (text . "When the Deferred is resolved, any doneCallbacks added by deferred.then
or deferred.done are called. Callbacks are executed in the order they
were added. Each callback is passed the args from the .resolve(). Any
doneCallbacks added after the Deferred enters the resolved state are
executed immediately when they are added, using the arguments that were
passed to the .resolve() call. For more information, see the
documentation for Deferred object.
") (text . "")) ("examples"))) jquery-doc-hash)

(push "$.sub" jquery-methods)

(puthash "$.sub" (quote (("name" . "$.sub") ("signatures" "$.sub" nil) ("desc" (text . "Creates a new copy of jQuery whose properties and methods can be
modified without affecting the original jQuery object.

")) ("longdesc" (text . "There are two specific use cases for which jQuery.sub() was created.
The first was for providing a painless way of overriding jQuery methods
without completely destroying the original methods and another was for
helping to do encapsulation and basic namespacing for jQuery plugins.
") (text . "") (text . "Note that jQuery.sub() doesnʼt attempt to do any sort of isolation -
thatʼs not its intention. All the methods on the subʼd version of
jQuery will still point to the original jQuery (events bound and
triggered will still be through the main jQuery, data will be bound to
elements through the main jQuery, Ajax queries and events will run
through the main jQuery, etc.).
") (text . "") (text . "Note that if youʼre looking to use this for plugin development you
should first strongly consider using something like the jQuery UI
widget factory which manages both state and plugin sub-methods. Some
examples of using the jQuery UI widget factory to build a plugin.
") (text . "") (text . "The particular use cases of this method can be best described through
some examples.

")) ("examples" ((text . "") (text . "Adding a method to a jQuery sub so that it isnʼt exposed externally:

") (text . "") (js . "  (function(){
	  var sub$ = jQuery.sub();
	  
	  sub$.fn.myCustomMethod = function(){
	  return 'just for me';
	  };
	  
	  sub$(document).ready(function() {
	  sub$('body').myCustomMethod() // 'just for me'
	  });
	  })();
	  
	  typeof jQuery('body').myCustomMethod // undefined") (text . "")) ((text . "") (text . "Override some jQuery methods to provide new functionality.

") (text . "") (js . "
	  (function() {
	  var myjQuery = jQuery.sub();
	  
	  myjQuery.fn.remove = function() {
	  // New functionality: Trigger a remove event
	  this.trigger(\"remove\");
	  
	  // Be sure to call the original jQuery remove method
	  return jQuery.fn.remove.apply( this, arguments );
	  };
	  
	  myjQuery(function($) {
	  $(\".menu\").click(function() {
	  $(this).find(\".submenu\").remove();
	  });
	  
	  // A new remove event is now triggered from this copy of jQuery
	  $(document).bind(\"remove\", function(e) {
	  $(e.target).parent().hide();
	  });
	  });
	  })();
	  
	  // Regular jQuery doesn't trigger a remove event when removing an element
	  // This functionality is only contained within the modified 'myjQuery'.") (text . "")) ((text . "") (text . "Create a plugin that returns plugin-specific methods.

") (text . "") (js . "
	  (function() {
	  // Create a new copy of jQuery using sub()
	  var plugin = jQuery.sub();
	  
	  // Extend that copy with the new plugin methods
	  plugin.fn.extend({
	  open: function() {
	  return this.show();
	  },
	  close: function() {
	  return this.hide();
	  }
	  });
	  
	  // Add our plugin to the original jQuery
	  jQuery.fn.myplugin = function() {
	  this.addClass(\"plugin\");
	  
	  // Make sure our plugin returns our special plugin version of jQuery
	  return plugin( this );
	  };
	  })();
	  
	  $(document).ready(function() {
	  // Call the plugin, open method now exists
	  $('#main').myplugin().open();
	  
	  // Note: Calling just $(\"#main\").open() won't work as open doesn't exist!
	  });") (text . ""))))) jquery-doc-hash)

(push "fadeToggle" jquery-methods)

(puthash "fadeToggle" (quote (("name" . "fadeToggle") ("signatures" "fadeToggle" (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Display or hide the matched elements by animating their opacity.

")) ("longdesc" (text . "") (text . "The .fadeToggle() method animates the opacity of the matched elements.
When called on a visible element, the elementʼs display style property
is set to none once the opacity reaches 0, so the element no longer
affects the layout of the page.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively.
") (text . "") (text . "The string representing an easing function specifies the speed at which
the animation progresses at different points within the animation. The
only easing implementations in the jQuery library are the default,
called swing, and one that progresses at a constant pace, called
linear. More easing functions are available with the use of plug-ins,
most notably the jQuery UI suite.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "")) ("examples" ((text . "") (text . "Fades first paragraph in or out, completing the animation within 600
milliseconds and using a linear easing. Fades last paragraph in or out
for 200 milliseconds, inserting a \"finished\" message upon completion.
") (text . "") (js . "
	  $(\"button:first\").click(function() {
	  $(\"p:first\").fadeToggle(\"slow\", \"linear\");
	  });
	  $(\"button:last\").click(function () {
	  $(\"p:last\").fadeToggle(\"fast\", function () {
	  $(\"#log\").append(\"<div>finished</div>\");
	  });
	  });
	  ") (text . "") (html . "
	  <button>fadeToggle p1</button>
	  <button>fadeToggle p2</button>
	  <p>This paragraph has a slow, linear fade.</p>
	  
	  <p>This paragraph has a fast animation.</p>
	  <div id=\"log\"></div>
	  ") (text . ""))))) jquery-doc-hash)

(push "$.type" jquery-methods)

(puthash "$.type" (quote (("name" . "$.type") ("signatures" "$.type" (("obj" "Object to get the internal JavaScript [[Class]] of.

" nil nil))) ("desc" (text . "Determine the internal JavaScript [[Class]] of an object.

")) ("longdesc" (text . "A number of techniques are used to determine the exact return value for
an object. The [[Class]] is determined as follows:

") (text . "") (text . "  * If the object is undefined or null, then \"undefined\" or \"null\" is
    returned accordingly.
  * If the object has an internal [[Class]] equivalent to one of the
    browserʼs built-in objects, the associated name is returned. ( More
    details about this technique.)
       + jQuery.type(true) === \"boolean\"
       + jQuery.type(3) === \"number\"
       + jQuery.type(\"test\") === \"string\"
       + jQuery.type(function(){}) === \"function\"
       + jQuery.type([]) === \"array\"
       + jQuery.type(new Date()) === \"date\"
       + jQuery.type(/test/) === \"regexp\"
  * Everything else returns \"object\" as its type.
") (text . "")) ("examples" ((text . "") (text . "Find out if the parameter is a RegExp.

") (text . "") (js . "$(\"b\").append( \"\" + jQuery.type(/test/) );") (text . "") (html . "Is it a RegExp? <b></b>") (text . ""))))) jquery-doc-hash)

(push "$.isWindow" jquery-methods)

(puthash "$.isWindow" (quote (("name" . "$.isWindow") ("signatures" "$.isWindow" (("obj" "Object to test whether or not it is a window.

" nil nil))) ("desc" (text . "Determine whether the argument is a window.

")) ("longdesc" (text . "This is used in a number of places in jQuery to determine if weʼre
operating against a browser window (such as the current window or an
iframe).
")) ("examples" ((text . "") (text . "Finds out if the parameter is a window.

") (text . "") (js . "$(\"b\").append( \"\" + $.isWindow(window) );") (text . "") (html . "Is 'window' a window? <b></b>") (text . ""))))) jquery-doc-hash)

(push "toggle" jquery-methods)

(puthash "toggle" (quote (("name" . "toggle") ("signatures" "toggle" (("handler(eventObject)" "A function to execute every even time the element is clicked.

" nil nil) ("handler(eventObject)" "A function to execute every odd time the element is clicked.

" nil nil) ("handler(eventObject)" "Additional handlers to cycle through after clicks.

" "true" nil))) ("desc" (text . "Bind two or more handlers to the matched elements, to be executed on
alternate clicks.

")) ("longdesc" (text . "") (text . "The .toggle() method binds a handler for the click event, so the rules
outlined for the triggering of click apply here as well.

") (text . "") (js . "For example, consider the HTML:
	  <div id=\"target\">
	  Click here
	  </div>") (text . "") (text . "

") (text . "") (text . "Event handlers can then be bound to the <div>:


") (text . "") (js . "$('#target').toggle(function() {
	  alert('First handler for .toggle() called.');
	  }, function() {
	  alert('Second handler for .toggle() called.');
	  });") (text . "") (text . "As the element is clicked repeatedly, the messages alternate:


") (text . "") (text . "First handler for .toggle() called.
Second handler for .toggle() called.
First handler for .toggle() called.
Second handler for .toggle() called.
First handler for .toggle() called.
") (text . "") (text . "If more than two handlers are provided, .toggle() will cycle among all
of them. For example, if there are three handlers, then the first
handler will be called on the first click, the fourth click, the
seventh click, and so on.
") (text . "") (text . "  Note: jQuery also provides an animation method named .toggle() that
  toggles the visibility of elements. Whether the animation or the
  event method is fired depends on the set of arguments passed.
") (text . "") (text . "The .toggle() method is provided for convenience. It is relatively
straightforward to implement the same behavior by hand, and this can be
necessary if the assumptions built into .toggle() prove limiting. For
example, .toggle() is not guaranteed to work correctly if applied twice
to the same element. Since .toggle() internally uses a click handler to
do its work, we must unbind click to remove a behavior attached with
.toggle(), so other click handlers can be caught in the crossfire. The
implementation also calls .preventDefault() on the event, so links will
not be followed and buttons will not be clicked if .toggle() has been
called on the element.
") (text . "")) ("examples" ((text . "") (text . "Click to toggle highlight on the list item.

") (text . "") (js . "
	  $(\"li\").toggle(
	  function () {
	  $(this).css({\"list-style-type\":\"disc\", \"color\":\"blue\"});
	  },
	  function () {
	  $(this).css({\"list-style-type\":\"disc\", \"color\":\"red\"});
	  },
	  function () {
	  $(this).css({\"list-style-type\":\"\", \"color\":\"\"});
	  }
	  );

	  ") (text . "") (css . "
	  ul { margin:10px; list-style:inside circle; font-weight:bold; }
	  li { cursor:pointer; }
	  ") (text . "") (html . "<ul>
	  <li>Go to the store</li>
	  <li>Pick up dinner</li>
	  <li>Debug crash</li>

	  <li>Take a jog</li>
	  </ul>") (text . "")) ((text . "") (text . "To toggle a style on table cells:

") (text . "") (js . "$(\"td\").toggle(
	  function () {
	  $(this).addClass(\"selected\");
	  },
	  function () {
	  $(this).removeClass(\"selected\");
	  }
	  );") (text . ""))))) jquery-doc-hash)

(push "undelegate" jquery-methods)

(puthash "undelegate" (quote (("name" . "undelegate") ("signatures" "undelegate" nil (("selector" "A selector which will be used to filter the event results.

" nil nil) ("eventType" "A string containing a JavaScript event type, such as \"click\" or
\"keydown\"

" nil nil)) (("selector" "A selector which will be used to filter the event results.

" nil nil) ("eventType" "A string containing a JavaScript event type, such as \"click\" or
\"keydown\"

" nil nil) ("handler" "A function to execute at the time the event is triggered.

" nil nil)) (("selector" "A selector which will be used to filter the event results.

" nil nil) ("events" "A map of one or more event types and previously bound functions to
unbind from them.

" nil nil)) (("namespace" "A string containing a namespace to unbind all events from.

" nil nil))) ("desc" (text . "Remove a handler from the event for all elements which match the
current selector, now or in the future, based upon a specific set of
root elements.
")) ("longdesc" (text . "") (text . "Undelegate is a way of removing event handlers that have been bound
using .delegate(). It works virtually identically to .die() with the
addition of a selector filter argument (which is required for
delegation to work).
") (text . "")) ("examples" ((text . "") (text . "Can bind and unbind events to the colored button.

") (text . "") (js . "
	  function aClick() {
	  $(\"div\").show().fadeOut(\"slow\");
	  }
	  $(\"#bind\").click(function () {
	  $(\"body\").delegate(\"#theone\", \"click\", aClick)
	  .find(\"#theone\").text(\"Can Click!\");
	  });
	  $(\"#unbind\").click(function () {
	  $(\"body\").undelegate(\"#theone\", \"click\", aClick)
	  .find(\"#theone\").text(\"Does nothing...\");
	  });
	  ") (text . "") (css . "
	  button { margin:5px; }
	  button#theone { color:red; background:yellow; }
	  ") (text . "") (html . "<button id=\"theone\">Does nothing...</button>
	  <button id=\"bind\">Bind Click</button>
	  <button id=\"unbind\">Unbind Click</button>
	  <div style=\"display:none;\">Click!</div>") (text . "")) ((text . "") (text . "To unbind all delegated events from all paragraphs, write:

") (text . "") (js . "$(\"p\").undelegate()") (text . "")) ((text . "") (text . "To unbind all delegated click events from all paragraphs, write:

") (text . "") (js . "$(\"p\").undelegate( \"click\" )") (text . "")) ((text . "") (text . "To undelegate just one previously bound handler, pass the function in
as the third argument:

") (text . "") (js . "var foo = function () {
	  // code to handle some kind of event
	  };
	  
	  // ... now foo will be called when paragraphs are clicked ...
	  $(\"body\").delegate(\"p\", \"click\", foo);
	  
	  
	  // ... foo will no longer be called.
	  $(\"body\").undelegate(\"p\", \"click\", foo); ") (text . "")) ((text . "") (text . "To unbind all delegated events by their namespace:

") (text . "") (js . "var foo = function () {
	  // code to handle some kind of event
	  };
	  
	  // delegate events under the \".whatever\" namespace
	  $(\"form\").delegate(\"click.whatever\", \":button\", foo);
	  
	  $(\"form\").delegate(\"keypress.whatever\", \":text\", foo); 
	  
	  // unbind all events delegated under the \".whatever\" namespace
	  
	  $(\"form\").undelegate(\".whatever\");") (text . ""))))) jquery-doc-hash)

(push "delegate" jquery-methods)

(puthash "delegate" (quote (("name" . "delegate") ("signatures" "delegate" (("selector" "A selector to filter the elements that trigger the event.

" nil nil) ("eventType" "A string containing one or more space-separated JavaScript event types,
such as \"click\" or \"keydown,\" or custom event names.

" nil nil) ("handler" "A function to execute at the time the event is triggered.

" nil nil)) (("selector" "A selector to filter the elements that trigger the event.

" nil nil) ("eventType" "A string containing one or more space-separated JavaScript event types,
such as \"click\" or \"keydown,\" or custom event names.

" nil nil) ("eventData" "A map of data that will be passed to the event handler.

" nil nil) ("handler" "A function to execute at the time the event is triggered.

" nil nil)) (("selector" "A selector to filter the elements that trigger the event.

" nil nil) ("events" "A map of one or more event types and functions to execute for them.

" nil nil))) ("desc" (text . "Attach a handler to one or more events for all elements that match the
selector, now or in the future, based on a specific set of root
elements.
")) ("longdesc" (text . "") (text . "Delegate is an alternative to using the .live() method, allowing for
each binding of event delegation to specific DOM elements. For example
the following delegate code:
") (text . "") (js . "$(\"table\").delegate(\"td\", \"hover\", function(){
	  $(this).toggleClass(\"hover\");
	  });") (text . "") (text . "Is equivalent to the following code written using .live():


") (text . "") (js . "$(\"table\").each(function(){
	  $(\"td\", this).live(\"hover\", function(){
	  $(this).toggleClass(\"hover\");
	  });
	  });") (text . "") (text . "See also the .undelegate() method for a way of removing event handlers
added in .delegate().

") (text . "") (text . "Passing and handling event data works the same way as it does for
.bind().

") (text . "")) ("examples" ((text . "") (text . "Click a paragraph to add another. Note that .delegate() binds the click
event to all paragraphs - even new ones.

") (text . "") (js . "
	  $(\"body\").delegate(\"p\", \"click\", function(){
	  $(this).after(\"<p>Another paragraph!</p>\");
	  });
	  ") (text . "") (css . "
	  p { background:yellow; font-weight:bold; cursor:pointer; 
	  padding:5px; }
	  p.over { background: #ccc; }
	  span { color:red; }
	  ") (text . "") (html . "<p>Click me!</p>
	  
	  <span></span>") (text . "")) ((text . "") (text . "To display each paragraphʼs text in an alert box whenever it is
clicked:

") (text . "") (js . "$(\"body\").delegate(\"p\", \"click\", function(){
	  alert( $(this).text() );
	  });") (text . "")) ((text . "") (text . "To cancel a default action and prevent it from bubbling up, return
false:

") (text . "") (js . "$(\"body\").delegate(\"a\", \"click\", function() { return false; })") (text . "")) ((text . "") (text . "To cancel only the default action by using the preventDefault method.

") (text . "") (js . "$(\"body\").delegate(\"a\", \"click\", function(event){
	  event.preventDefault();
	  });") (text . "")) ((text . "") (text . "Can bind custom events too.

") (text . "") (js . "
	  
	  $(\"body\").delegate(\"p\", \"myCustomEvent\", function(e, myName, myValue){
	  $(this).text(\"Hi there!\");
	  $(\"span\").stop().css(\"opacity\", 1)
	  .text(\"myName = \" + myName)
	  .fadeIn(30).fadeOut(1000);
	  });
	  $(\"button\").click(function () {
	  $(\"p\").trigger(\"myCustomEvent\");
	  });
	  
	  ") (text . "") (css . "
	  p { color:red; }
	  span { color:blue; }
	  ") (text . "") (html . "<p>Has an attached custom event.</p>
	  <button>Trigger custom event</button>
	  <span style=\"display:none;\"></span>") (text . ""))))) jquery-doc-hash)

(push "$.error" jquery-methods)

(puthash "$.error" (quote (("name" . "$.error") ("signatures" "$.error" (("message" "The message to send out.

" nil nil))) ("desc" (text . "Takes a string and throws an exception containing it.

")) ("longdesc" (text . "This method exists primarily for plugin developers who wish to override
it and provide a better display (or more information) for the error
messages.
")) ("examples" ((text . "") (text . "Override jQuery.error for display in Firebug.

") (text . "") (js . "jQuery.error = console.error;") (text . ""))))) jquery-doc-hash)

(push "$.parseJSON" jquery-methods)

(puthash "$.parseJSON" (quote (("name" . "$.parseJSON") ("signatures" "$.parseJSON" (("json" "The JSON string to parse.

" nil nil))) ("desc" (text . "Takes a well-formed JSON string and returns the resulting JavaScript
object.

")) ("longdesc" (text . "Passing in a malformed JSON string may result in an exception being
thrown. For example, the following are all malformed JSON strings:

") (text . "") (text . "  * {test: 1} (test does not have double quotes around it).
  * {ʼtestʼ: 1} (ʼtestʼ is using single quotes instead of double
    quotes).
") (text . "") (text . "Additionally if you pass in nothing, an empty string, null, or
undefined, ʼnullʼ will be returned from parseJSON. Where the browser
provides a native implementation of JSON.parse, jQuery uses it to parse
the string. For details on the JSON format, see http://json.org/.
")) ("examples" ((text . "") (text . "Parse a JSON string.

") (text . "") (js . "var obj = jQuery.parseJSON('{\"name\":\"John\"}');
	    alert( obj.name === \"John\" );") (text . ""))))) jquery-doc-hash)

(push "$.proxy" jquery-methods)

(puthash "$.proxy" (quote (("name" . "$.proxy") ("signatures" "$.proxy" (("function" "The function whose context will be changed.

" nil nil) ("context" "The object to which the context ( this) of the function should be set.

" nil nil)) (("context" "The object to which the context of the function should be set.

" nil nil) ("name" "The name of the function whose context will be changed (should be a
property of the context object).

" nil nil))) ("desc" (text . "Takes a function and returns a new one that will always have a
particular context.

")) ("longdesc" (text . "This method is most useful for attaching event handlers to an element
where the context is pointing back to a different object. Additionally,
jQuery makes sure that even if you bind the function returned from
jQuery.proxy() it will still unbind the correct function if passed the
original.
")) ("examples" ((text . "") (text . "Enforce the context of the function using the \"function, context\"
signature. Unbind the handler after first click.

") (text . "") (html . "
	    <p><button id=\"test\">Test</button></p>
	    <p id=\"log\"></p>
	    ") (text . "") (js . "
	    var obj = {
	    name: \"John\",
	    test: function() {
	    $(\"#log\").append( this.name );
	    $(\"#test\").unbind(\"click\", obj.test);
	    }
	    };
	    
	    $(\"#test\").click( jQuery.proxy( obj.test, obj ) );
	    ") (text . "")) ((text . "") (text . "Enforce the context of the function using the \"context, function name\"
signature. Unbind the handler after first click.

") (text . "") (html . "
	    <p><button id=\"test\">Test</button></p>
	    <p id=\"log\"></p>
	    ") (text . "") (js . "
	    var obj = {
	    name: \"John\",
	    test: function() {
	    $(\"#log\").append( this.name );
	    $(\"#test\").unbind(\"click\", obj.test);
	    }
	    };
	    
	    $(\"#test\").click( jQuery.proxy( obj, \"test\" ) );
	    ") (text . ""))))) jquery-doc-hash)

(push "focusout" jquery-methods)

(puthash "focusout" (quote (("name" . "focusout") ("signatures" "focusout" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil))) ("desc" (text . "Bind an event handler to the \"focusout\" JavaScript event.

")) ("longdesc" (text . "This method is a shortcut for .bind(ʼfocusoutʼ, handler).


") (text . "") (text . "The focusout event is sent to an element when it, or any element inside
of it, loses focus. This is distinct from the blur event in that it
supports detecting the loss of focus from parent elements (in other
words, it supports event bubbling).
") (text . "") (text . "This event will likely be used together with the focusin event.


")) ("examples" ((text . "") (text . "Watch for a loss of focus to occur inside paragraphs and note the
difference between the focusout count and the blur count.

") (text . "") (css . "
	    .inputs { float: left; margin-right: 1em; }
	    .inputs p { margin-top: 0; }
	    ") (text . "") (js . "
	    var fo = 0, b = 0;
	    $(\"p\").focusout(function() {
	    fo++;
	    $(\"#fo\")
	    .text(\"focusout fired: \" + fo + \"x\");
	    }).blur(function() {
	    b++;
	    $(\"#b\")
	    .text(\"blur fired: \" + b + \"x\");
	    
	    });
	    ") (text . "") (html . "
	    <div class=\"inputs\">
	    <p>
	    <input type=\"text\" /><br />
	    <input type=\"text\" /> 
	    </p>
	    <p>
	    <input type=\"password\" />
	    </p>
	    </div>
	    <div id=\"fo\">focusout fire</div>
	    <div id=\"b\">blur fire</div>
	    ") (text . ""))))) jquery-doc-hash)

(push "focusin" jquery-methods)

(puthash "focusin" (quote (("name" . "focusin") ("signatures" "focusin" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil))) ("desc" (text . "Bind an event handler to the \"focusin\" JavaScript event.

")) ("longdesc" (text . "This method is a shortcut for .bind(ʼfocusinʼ, handler).


") (text . "") (text . "The focusin event is sent to an element when it, or any element inside
of it, gains focus. This is distinct from the focus event in that it
supports detecting the focus event on parent elements (in other words,
it supports event bubbling).
") (text . "") (text . "This event will likely be used together with the focusout event.


")) ("examples" ((text . "") (text . "Watch for a focus to occur within the paragraphs on the page.

") (text . "") (css . "span {display:none;}") (text . "") (js . "
	    $(\"p\").focusin(function() {
	    $(this).find(\"span\").css('display','inline').fadeOut(1000);
	    });
	    ") (text . "") (html . "<p><input type=\"text\" /> <span>focusin fire</span></p>
	    <p><input type=\"password\" /> <span>focusin fire</span></p>") (text . ""))))) jquery-doc-hash)

(push "has" jquery-methods)

(puthash "has" (quote (("name" . "has") ("signatures" "has" (("selector" "A string containing a selector expression to match elements against.

" nil nil)) (("contained" "A DOM element to match elements against.

" nil nil))) ("desc" (text . "Reduce the set of matched elements to those that have a descendant that
matches the selector or DOM element.

")) ("longdesc" (text . "") (text . "Given a jQuery object that represents a set of DOM elements, the .has()
method constructs a new jQuery object from a subset of the matching
elements. The supplied selector is tested against the descendants of
the matching elements; the element will be included in the result if
any of its descendant elements matches the selector.
") (text . "") (text . "Consider a page with a nested list as follows:


") (text . "") (js . "
	      <ul>
	      <li>list item 1</li>
	      <li>list item 2
	      <ul>
	      <li>list item 2-a</li>
	      <li>list item 2-b</li>
	      </ul>
	      </li>
	      <li>list item 3</li>
	      <li>list item 4</li>
	      </ul>
	    ") (text . "") (text . "We can apply this method to the set of list items as follows:


") (text . "") (js . "$('li').has('ul').css('background-color', 'red');") (text . "") (text . "The result of this call is a red background for item 2, as it is the
only <li> that has a <ul> among its descendants.

") (text . "")) ("examples" ((text . "") (text . "Check if an element is inside another.

") (text . "") (js . "
	    $(\"ul\").append(\"<li>\" + ($(\"ul\").has(\"li\").length ? \"Yes\" : \"No\") + \"</li>\");
	    $(\"ul\").has(\"li\").addClass(\"full\");
	    ") (text . "") (css . "
	    .full { border: 1px solid red; }
	    ") (text . "") (html . "
	    <ul><li>Does the UL contain an LI?</li></ul>
	    ") (text . ""))))) jquery-doc-hash)

(push "$.contains" jquery-methods)

(puthash "$.contains" (quote (("name" . "$.contains") ("signatures" "$.contains" (("container" "The DOM element that may contain the other element.

" nil nil) ("contained" "The DOM node that may be contained by the other element.

" nil nil))) ("desc" (text . "Check to see if a DOM node is within another DOM node.

")) ("longdesc") ("examples" ((text . "") (text . "Check if an element is inside another.

") (text . "") (js . "jQuery.contains(document.documentElement, document.body); // true
	    jQuery.contains(document.body, document.documentElement); // false") (text . ""))))) jquery-doc-hash)

(push "$.noop" jquery-methods)

(puthash "$.noop" (quote (("name" . "$.noop") ("signatures" "$.noop" nil) ("desc" (text . "An empty function.

")) ("longdesc" (text . "You can use this empty function when you wish to pass around a function
that will do nothing.

") (text . "") (text . "This is useful for plugin authors who offer optional callbacks; in the
case that no callback is given, something like jQuery.noop could
execute.
")) ("examples"))) jquery-doc-hash)

(push "delay" jquery-methods)

(puthash "delay" (quote (("name" . "delay") ("signatures" "delay" (("duration" "An integer indicating the number of milliseconds to delay execution of
the next item in the queue.

" nil nil) ("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" "true" nil))) ("desc" (text . "Set a timer to delay execution of subsequent items in the queue.

")) ("longdesc" (text . "Added to jQuery in version 1.4, the .delay() method allows us to delay
the execution of functions that follow it in the queue. It can be used
with the standard effects queue or with a custom queue. Only subsequent
events in a queue are delayed; for example this will not delay the
no-arguments forms of .show() or .hide() which do not use the effects
queue.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively.
") (text . "") (text . "Using the standard effects queue, we can, for example, set an
800-millisecond delay between the .slideUp() and .fadeIn() of <div
id=\"foo\">:
") (text . "") (js . "$('#foo').slideUp(300).delay(800).fadeIn(400);") (text . "") (text . "When this statement is executed, the element slides up for 300
milliseconds and then pauses for 800 milliseconds before fading in for
400 milliseconds.
") (text . "") (text . "  The .delay() method is best for delaying between queued jQuery
  effects. Because it is limitedâit doesnʼt, for example, offer a way
  to cancel the delayâ .delay() is not a replacement for JavaScriptʼs
  native setTimeout function, which may be more appropriate for
  certain use cases.
") (text . "")) ("examples" ((text . "") (text . "Animate the hiding and showing of two divs, delaying the first before
showing it.

") (text . "") (css . "
	    div { width: 60px; height: 60px; float: left; }
	    .first { background-color: #3f3; }
	    .second { background-color: #33f;}
	    ") (text . "") (js . "
	    $(\"button\").click(function() {
	    $(\"div.first\").slideUp(300).delay(800).fadeIn(400);
	    $(\"div.second\").slideUp(300).fadeIn(400);
	    });
	    ") (text . "") (html . "
	    <p><button>Run</button></p>
	    <div class=\"first\"></div>
	    <div class=\"second\"></div>
	    ") (text . ""))))) jquery-doc-hash)

(push "parentsUntil" jquery-methods)

(puthash "parentsUntil" (quote (("name" . "parentsUntil") ("signatures" "parentsUntil" (("selector" "A string containing a selector expression to indicate where to stop
matching ancestor elements.

" "true" nil))) ("desc" (text . "Get the ancestors of each element in the current set of matched
elements, up to but not including the element matched by the selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.parentsUntil() method traverses through the ancestors of these
elements until it reaches an element matched by the selector passed in
the methodʼs argument. The resulting jQuery object contains all of the
ancestors up to but not including the one matched by the
.parentsUntil() selector. Consider a page with a basic nested list as
follows:
") (text . "") (js . "<ul class=\"level-1\">
	  <li class=\"item-i\">I</li>
	  <li class=\"item-ii\">II
	  <ul class=\"level-2\">
	  <li class=\"item-a\">A</li>
	  <li class=\"item-b\">B
	  <ul class=\"level-3\">
	  <li class=\"item-1\">1</li>
	  <li class=\"item-2\">2</li>
	  <li class=\"item-3\">3</li>
	  </ul>
	  </li>
	  <li class=\"item-c\">C</li>
	  </ul>
	  </li>
	  <li class=\"item-iii\">III</li>
	  </ul>
	  ") (text . "") (text . "If we begin at item A, we can find its ancestors up to but not
including <ul class=\"level-1\"> as follows:

") (text . "") (js . "$('li.item-a').parentsUntil('.level-1')
	  .css('background-color', 'red');") (text . "") (text . "The result of this call is a red background for the level-2 list and
the item II.

") (text . "") (text . "If the .parentsUntil() selector is not matched, or if no selector is
supplied, the returned jQuery object contains all of the previous
jQuery objectʼs ancestors. For example, letʼs say we begin at item A
again, but this time we use a selector that is not matched by any of
its ancestors:
") (text . "") (js . "$('li.item-a').parentsUntil('.not-here')
	  .css('background-color', 'red');") (text . "") (text . "The result of this call is a red background-color style applied to the
level-2 list, the item II, the level-1 list, the <body> element, and
the <html> element.
") (text . "")) ("examples" ((text . "") (text . "Find the ancestors of <li class=\"item-a\"> up to <ul class=\"level-1\">
and give them a red background color.

") (text . "") (js . "
	    $('li.item-a').parentsUntil('.level-1')
	    .css('background-color', 'red');
	    ") (text . "") (html . "
	    <ul class=\"level-1\">
	    <li class=\"item-i\">I</li>
	    <li class=\"item-ii\">II
	    <ul class=\"level-2\">
	    <li class=\"item-a\">A</li>
	    <li class=\"item-b\">B
	    <ul class=\"level-3\">
	    <li class=\"item-1\">1</li>
	    <li class=\"item-2\">2</li>
	    <li class=\"item-3\">3</li>
	    </ul>
	    </li>
	    <li class=\"item-c\">C</li>
	    </ul>
	    </li>
	    <li class=\"item-iii\">III</li>
	    </ul>") (text . ""))))) jquery-doc-hash)

(push "prevUntil" jquery-methods)

(puthash "prevUntil" (quote (("name" . "prevUntil") ("signatures" "prevUntil" (("selector" "A string containing a selector expression to indicate where to stop
matching preceding sibling elements.

" "true" nil))) ("desc" (text . "Get all preceding siblings of each element up to but not including the
element matched by the selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.prevUntil() method searches through the predecessors of these elements
in the DOM tree, stopping when it reaches an element matched by the
methodʼs argument. The new jQuery object that is returned contains all
previous siblings up to but not including the one matched by the
.prevUntil() selector; the elements are returned in order from the
closest sibling to the farthest.
") (text . "") (text . "If the selector is not matched or is not supplied, all previous
siblings will be selected; in these cases it selects the same elements
as the .prevAll() method does when no filter selector is provided.
") (text . "") (text . "Consider a page with a simple definition list as follows:


") (text . "") (js . "
	    <dl>
	    <dt>term 1</dt>
	    <dd>definition 1-a</dd>
	    <dd>definition 1-b</dd>
	    <dd>definition 1-c</dd>
	    <dd>definition 1-d</dd>
	    
	    <dt id=\"term-2\">term 2</dt>
	    <dd>definition 2-a</dd>
	    <dd>definition 2-b</dd>
	    <dd>definition 2-c</dd>
	    
	    <dt>term 3</dt>
	    <dd>definition 3-a</dd>
	    <dd>definition 3-b</dd>
	    </dl>
	  ") (text . "") (text . "If we begin at the second term, we can find the elements which come
after it until a preceding <dt>.

") (text . "") (js . "$('#term-2').prevUntil('dt').css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind definitions 1-a,
1-b, 1-c, and 1-d.

") (text . "")) ("examples" ((text . "") (text . "Find the siblings that precede <dt id=\"term-2\"> up to the preceding
<dt> and give them a red background color.

") (text . "") (js . "
	    $(\"#term-2\").prevUntil(\"dt\")
	    .css(\"background-color\", \"red\")
	    ") (text . "") (html . "<dl>
	    <dt>term 1</dt>
	    <dd>definition 1-a</dd>
	    <dd>definition 1-b</dd>
	    <dd>definition 1-c</dd>
	    <dd>definition 1-d</dd>
	    
	    <dt id=\"term-2\">term 2</dt>
	    <dd>definition 2-a</dd>
	    <dd>definition 2-b</dd>
	    <dd>definition 2-c</dd>
	    
	    <dt>term 3</dt>
	    <dd>definition 3-a</dd>
	    <dd>definition 3-b</dd>
	    </dl>") (text . ""))))) jquery-doc-hash)

(push "nextUntil" jquery-methods)

(puthash "nextUntil" (quote (("name" . "nextUntil") ("signatures" "nextUntil" (("selector" "A string containing a selector expression to indicate where to stop
matching following sibling elements.

" "true" nil))) ("desc" (text . "Get all following siblings of each element up to but not including the
element matched by the selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.nextUntil() method allows us to search through the successors of these
elements in the DOM tree, stopping when it reaches an element matched
by the methodʼs argument. The new jQuery object that is returned
contains all following siblings up to but not including the one matched
by the .nextUntil() selector.
") (text . "") (text . "If the selector is not matched or is not supplied, all following
siblings will be selected; in these cases it selects the same elements
as the .nextAll() method does when no filter selector is provided.
") (text . "") (text . "Consider a page with a simple definition list as follows:


") (text . "") (js . "
	    <dl>
	    <dt>term 1</dt>
	    <dd>definition 1-a</dd>
	    <dd>definition 1-b</dd>
	    <dd>definition 1-c</dd>
	    <dd>definition 1-d</dd>
	    
	    <dt id=\"term-2\">term 2</dt>
	    <dd>definition 2-a</dd>
	    <dd>definition 2-b</dd>
	    <dd>definition 2-c</dd>
	    
	    <dt>term 3</dt>
	    <dd>definition 3-a</dd>
	    <dd>definition 3-b</dd>
	    </dl>
	  ") (text . "") (text . "If we begin at the second term, we can find the elements which come
after it until a following <dt>.

") (text . "") (js . "$('#term-2').nextUntil('dt').css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind definitions 2-a,
2-b, and 2-c.

") (text . "")) ("examples" ((text . "") (text . "Find the siblings that follow <dt id=\"term-2\"> up to the next <dt> and
give them a red background color.

") (text . "") (js . "
	    $(\"#term-2\").nextUntil(\"dt\")
	    .css(\"background-color\", \"red\")
	    ") (text . "") (html . "<dl>
	    <dt>term 1</dt>
	    <dd>definition 1-a</dd>
	    <dd>definition 1-b</dd>
	    <dd>definition 1-c</dd>
	    <dd>definition 1-d</dd>
	    
	    <dt id=\"term-2\">term 2</dt>
	    <dd>definition 2-a</dd>
	    <dd>definition 2-b</dd>
	    <dd>definition 2-c</dd>
	    
	    <dt>term 3</dt>
	    <dd>definition 3-a</dd>
	    <dd>definition 3-b</dd>
	    </dl>") (text . ""))))) jquery-doc-hash)

(push "event.isImmediatePropagationStopped" jquery-methods)

(puthash "event.isImmediatePropagationStopped" (quote (("name" . "event.isImmediatePropagationStopped") ("signatures" "event.isImmediatePropagationStopped" nil) ("desc" (text . "Returns whether event.stopImmediatePropagation() was ever called on
this event object.

")) ("longdesc" (text . "") (text . "This property was introduced in DOM level 3.


") (text . "")) ("examples" ((text . "") (text . "Checks whether event.stopImmediatePropagation() was called.

") (text . "") (js . "
	    
	    function immediatePropStopped(e) {
	    var msg = \"\";
	    if ( e.isImmediatePropagationStopped() ) {
	    msg =  \"called\"
	    } else {
	    msg = \"not called\";
	    }
	    $(\"#stop-log\").append( \"<div>\" + msg + \"</div>\" );
	    }
	    
	    $(\"button\").click(function(event) {
	    immediatePropStopped(event);
	    event.stopImmediatePropagation();
	    immediatePropStopped(event);
	    });  
	    ") (text . "") (html . "
	    <button>click me</button>
	    <div id=\"stop-log\"></div>
	    ") (text . ""))))) jquery-doc-hash)

(push "event.stopImmediatePropagation" jquery-methods)

(puthash "event.stopImmediatePropagation" (quote (("name" . "event.stopImmediatePropagation") ("signatures" "event.stopImmediatePropagation" nil) ("desc" (text . "Keeps the rest of the handlers from being executed and prevents the
event from bubbling up the DOM tree.

")) ("longdesc" (text . "In addition to keeping any additional handlers on an element from being
executed, this method also stops the bubbling by implicitly calling
event.stopPropagation(). To simply prevent the event from bubbling to
ancestor elements but allow other event handlers to execute on the same
element, we can use event.stopPropagation() instead.
") (text . "") (text . "Use event.isImmediatePropagationStopped() to know whether this method
was ever called (on that event object).

") (text . "")) ("examples" ((text . "") (text . "Prevents other event handlers from being called.

") (text . "") (css . "
	    p { height: 30px; width: 150px; background-color: #ccf; }
	    div {height: 30px; width: 150px; background-color: #cfc; }
	    ") (text . "") (js . "
	    $(\"p\").click(function(event){
	    event.stopImmediatePropagation();
	    });
	    $(\"p\").click(function(event){
	    // This function won't be executed
	    $(this).css(\"background-color\", \"#f00\");
	    });  
	    $(\"div\").click(function(event) {
	    // This function will be executed
	    $(this).css(\"background-color\", \"#f00\");
	    });") (text . "") (html . "<p>paragraph</p>
	    <div>division</div>") (text . ""))))) jquery-doc-hash)

(push "event.isPropagationStopped" jquery-methods)

(puthash "event.isPropagationStopped" (quote (("name" . "event.isPropagationStopped") ("signatures" "event.isPropagationStopped" nil) ("desc" (text . "Returns whether event.stopPropagation() was ever called on this event
object.

")) ("longdesc" (text . "This event method is described in the W3C DOM Level 3 specification.


")) ("examples" ((text . "") (text . "Checks whether event.stopPropagation() was called

") (text . "") (js . "
	    
	    function propStopped(e) {
	    var msg = \"\";
	    if ( e.isPropagationStopped() ) {
	    msg =  \"called\"
	    } else {
	    msg = \"not called\";
	    }
	    $(\"#stop-log\").append( \"<div>\" + msg + \"</div>\" );
	    }
	    
	    $(\"button\").click(function(event) {
	    propStopped(event);
	    event.stopPropagation();
	    propStopped(event);
	    });  
	    ") (text . "") (html . "
	    <button>click me</button>
	    <div id=\"stop-log\"></div>
	    ") (text . ""))))) jquery-doc-hash)

(push "event.stopPropagation" jquery-methods)

(puthash "event.stopPropagation" (quote (("name" . "event.stopPropagation") ("signatures" "event.stopPropagation" nil) ("desc" (text . "Prevents the event from bubbling up the DOM tree, preventing any parent
handlers from being notified of the event.

")) ("longdesc" (text . "") (text . "We can use event.isPropagationStopped() to determine if this method was
ever called (on that event object).

") (text . "") (text . "This method works for custom events triggered with trigger(), as well.


") (text . "") (text . "Note that this will not prevent other handlers on the same element from
running.

") (text . "")) ("examples" ((text . "") (text . "Kill the bubbling on the click event.

") (text . "") (js . "$(\"p\").click(function(event){
	    event.stopPropagation();
	    // do something
	    });  ") (text . ""))))) jquery-doc-hash)

(push "event.isDefaultPrevented" jquery-methods)

(puthash "event.isDefaultPrevented" (quote (("name" . "event.isDefaultPrevented") ("signatures" "event.isDefaultPrevented" nil) ("desc" (text . "Returns whether event.preventDefault() was ever called on this event
object.

")) ("longdesc" (text . "")) ("examples" ((text . "") (text . "Checks whether event.preventDefault() was called.

") (text . "") (js . "$(\"a\").click(function(event){
	    alert( event.isDefaultPrevented() ); // false
	    event.preventDefault();
	    alert( event.isDefaultPrevented() ); // true
	    });  ") (text . ""))))) jquery-doc-hash)

(push "event.preventDefault" jquery-methods)

(puthash "event.preventDefault" (quote (("name" . "event.preventDefault") ("signatures" "event.preventDefault" nil) ("desc" (text . "If this method is called, the default action of the event will not be
triggered.

")) ("longdesc" (text . "") (text . "For example, clicked anchors will not take the browser to a new URL. We
can use event.isDefaultPrevented() to determine if this method has been
called by an event handler that was triggered by this event.
") (text . "")) ("examples" ((text . "") (text . "Cancel the default action (navigation) of the click.

") (text . "") (js . "
	    $(\"a\").click(function(event) {
	    event.preventDefault();
	    $('<div/>')
	    .append('default ' + event.type + ' prevented')
	    .appendTo('#log');
	    });
	    ") (text . "") (html . "
	    <a href=\"http://jquery.com\">default click action is prevented</a>
	    <div id=\"log\"></div>
	    ") (text . ""))))) jquery-doc-hash)

(push "each" jquery-methods)

(puthash "each" (quote (("name" . "each") ("signatures" "each" (("function(index, Element)" "A function to execute for each matched element.

" nil nil))) ("desc" (text . "Iterate over a jQuery object, executing a function for each matched
element.

")) ("longdesc" (text . "") (text . "The .each() method is designed to make DOM looping constructs concise
and less error-prone. When called it iterates over the DOM elements
that are part of the jQuery object. Each time the callback runs, it is
passed the current loop iteration, beginning from 0. More importantly,
the callback is fired in the context of the current DOM element, so the
keyword this refers to the element.
") (text . "") (text . "Suppose we had a simple unordered list on the page:


") (text . "") (js . "<ul>
	    <li>foo</li>
	    <li>bar</li>
	    </ul>
	    ") (text . "") (text . "We can select the list items and iterate across them:


") (text . "") (js . "$('li').each(function(index) {
	    alert(index + ': ' + $(this).text());
	    });
	    ") (text . "") (text . "A message is thus alerted for each item in the list:


") (text . "") (text . "0: foo
1: bar

") (text . "") (text . "We can stop the loop from within the callback function by returning
false.

") (text . "")) ("examples" ((text . "") (text . "Iterates over three divs and sets their color property.

") (text . "") (js . "
	    $(document.body).click(function () {
	    $(\"div\").each(function (i) {
	    if (this.style.color != \"blue\") {
	    this.style.color = \"blue\";
	    } else {
	    this.style.color = \"\";
	    }
	    });
	    });") (text . "") (css . "
	    div { color:red; text-align:center; cursor:pointer; 
	    font-weight:bolder; width:300px; }
	    ") (text . "") (html . "<div>Click here</div>

	    <div>to iterate through</div>
	    <div>these divs.</div>") (text . "")) ((text . "") (text . "If you want to have the jQuery object instead of the regular DOM
element, use the $(this) function, for example:

") (text . "") (js . "
	    $(\"span\").click(function () {
	    $(\"li\").each(function(){
	    $(this).toggleClass(\"example\");
	    });
	    });

	    ") (text . "") (css . "
	    ul { font-size:18px; margin:0; }
	    span { color:blue; text-decoration:underline; cursor:pointer; }
	    .example { font-style:italic; }
	    ") (text . "") (html . "To do list: <span>(click here to change)</span>
	    <ul>
	    <li>Eat</li>
	    <li>Sleep</li>

	    <li>Be merry</li>
	    </ul>") (text . "")) ((text . "") (text . "You can use ʼreturnʼ to break out of each() loops early.

") (text . "") (js . "
	    $(\"button\").click(function () {
	    $(\"div\").each(function (index, domEle) {
	    // domEle == this
	    $(domEle).css(\"backgroundColor\", \"yellow\"); 
	    if ($(this).is(\"#stop\")) {
	    $(\"span\").text(\"Stopped at div index #\" + index);
	    return false;
	    }
	    });
	    });

	    ") (text . "") (css . "
	    div { width:40px; height:40px; margin:5px; float:left;
	    border:2px blue solid; text-align:center; }
	    span { color:red; }
	    ") (text . "") (html . "<button>Change colors</button> 
	    <span></span>
	    <div></div>
	    <div></div>

	    <div></div>
	    <div></div>
	    <div id=\"stop\">Stop here</div>
	    <div></div>

	    <div></div>
	    <div></div>") (text . ""))))) jquery-doc-hash)

(push "pushStack" jquery-methods)

(puthash "pushStack" (quote (("name" . "pushStack") ("signatures" "pushStack" (("elements" "An array of elements to push onto the stack and make into a new jQuery
object.

" nil nil)) (("elements" "An array of elements to push onto the stack and make into a new jQuery
object.

" nil nil) ("name" "The name of a jQuery method that generated the array of elements.

" nil nil) ("arguments" "The arguments that were passed in to the jQuery method (for
serialization).

" nil nil))) ("desc" (text . "Add a collection of DOM elements onto the jQuery stack.

")) ("longdesc") ("examples" ((text . "") (text . "Add some elements onto the jQuery stack, then pop back off again.

") (text . "") (js . "jQuery([])
	    .pushStack( document.getElementsByTagName(\"div\") )
	    .remove()
	    .end();") (text . ""))))) jquery-doc-hash)

(push "$.globalEval" jquery-methods)

(puthash "$.globalEval" (quote (("name" . "$.globalEval") ("signatures" "$.globalEval" (("code" "The JavaScript code to execute.

" nil nil))) ("desc" (text . "Execute some JavaScript code globally.

")) ("longdesc" (text . "This method behaves differently from using a normal JavaScript eval()
in that itʼs executed within the global context (which is important for
loading external scripts dynamically).
")) ("examples" ((text . "") (text . "Execute a script in the global context.

") (text . "") (js . "function test(){
	    jQuery.globalEval(\"var newVar = true;\")
	    }
	    test();
	    // newVar === true") (text . ""))))) jquery-doc-hash)

(push "$.isXMLDoc" jquery-methods)

(puthash "$.isXMLDoc" (quote (("name" . "$.isXMLDoc") ("signatures" "$.isXMLDoc" (("node" "The DOM node that will be checked to see if itʼs in an XML document.

" nil nil))) ("desc" (text . "Check to see if a DOM node is within an XML document (or is an XML
document).

")) ("longdesc") ("examples" ((text . "") (text . "Check an object to see if itʼs in an XML document.

") (text . "") (js . "jQuery.isXMLDoc(document) // false
	    jQuery.isXMLDoc(document.body) // false") (text . ""))))) jquery-doc-hash)

(push "$.removeData" jquery-methods)

(puthash "$.removeData" (quote (("name" . "$.removeData") ("signatures" "$.removeData" (("element" "A DOM element from which to remove data.

" nil nil) ("name" "A string naming the piece of data to remove.

" "true" nil))) ("desc" (text . "Remove a previously-stored piece of data.

")) ("longdesc" (text . "Note: This is a low-level method, you should probably use .removeData()
instead.

") (text . "") (text . "The jQuery.removeData() method allows us to remove values that were
previously set using jQuery.data() . When called with the name of a
key, jQuery.removeData() deletes that particular value; when called
with no arguments, all values are removed.
")) ("examples" ((text . "") (text . "Set a data store for 2 names then remove one of them.

") (text . "") (js . "
	    var div = $(\"div\")[0];
	    $(\"span:eq(0)\").text(\"\" + $(\"div\").data(\"test1\"));
	    jQuery.data(div, \"test1\", \"VALUE-1\");
	    jQuery.data(div, \"test2\", \"VALUE-2\");
	    $(\"span:eq(1)\").text(\"\" + jQuery.data(div, \"test1\"));
	    jQuery.removeData(div, \"test1\");
	    $(\"span:eq(2)\").text(\"\" + jQuery.data(div, \"test1\"));
	    $(\"span:eq(3)\").text(\"\" + jQuery.data(div, \"test2\"));") (text . "") (css . "
	    div { margin:2px; color:blue; }
	    span { color:red; }
	    ") (text . "") (html . "<div>value1 before creation: <span></span></div>
	    <div>value1 after creation: <span></span></div>
	    <div>value1 after removal: <span></span></div>
	    <div>value2 after removal: <span></span></div>") (text . ""))))) jquery-doc-hash)

(push "$.data" jquery-methods)

(puthash "$.data" (quote (("name" . "$.data") ("signatures" "$.data" (("element" "The DOM element to associate with the data.

" nil nil) ("key" "A string naming the piece of data to set.

" nil nil) ("value" "The new data value.

" nil nil))) ("desc" (text . "Store arbitrary data associated with the specified element. Returns the
value that was set.

")) ("longdesc" (text . "Note: This is a low-level method; a more convenient .data() is also
available.

") (text . "") (text . "The jQuery.data() method allows us to attach data of any type to DOM
elements in a way that is safe from circular references and therefore
free from memory leaks. jQuery ensures that the data is removed when
DOM elements are removed via jQuery methods, and when the user leaves
the page. We can set several distinct values for a single element and
retrieve them later:
") (text . "") (js . "
	    jQuery.data(document.body, 'foo', 52);
	    jQuery.data(document.body, 'bar', 'test');
	  ") (text . "") (text . "Note: this method currently does not provide cross-platform support for
setting data on XML documents, as Internet Explorer does not allow data
to be attached via expando properties.
") (text . "")) ("examples" ((text . "") (text . "Store then retrieve a value from the div element.

") (text . "") (js . "var div = $(\"div\")[0];
	    jQuery.data(div, \"test\", { first: 16, last: \"pizza!\" });
	    $(\"span:first\").text(jQuery.data(div, \"test\").first);
	    $(\"span:last\").text(jQuery.data(div, \"test\").last);") (text . "") (css . "
	    div { color:blue; }
	    span { color:red; }
	    ") (text . "") (html . "<div>
	    The values stored were 
	    <span></span>
	    and
	    <span></span>
	    </div>") (text . ""))))) jquery-doc-hash)

(push "$.data" jquery-methods)

(puthash "$.data" (quote (("name" . "$.data") ("signatures" "$.data" (("element" "The DOM element to query for the data.

" nil nil) ("key" "Name of the data stored.

" nil nil)) (("element" "The DOM element to query for the data.

" nil nil))) ("desc" (text . "Returns value at named data store for the element, as set by
jQuery.data(element, name, value), or the full data store for the
element.
")) ("longdesc" (text . "Note: This is a low-level method; a more convenient .data() is also
available.

") (text . "") (text . "Regarding HTML5 data-* attributes: This low-level method does NOT
retrieve the data-* attributes unless the more convenient .data()
method has already retrieved them.
") (text . "") (text . "The jQuery.data() method allows us to attach data of any type to DOM
elements in a way that is safe from circular references and therefore
from memory leaks. We can retrieve several distinct values for a single
element one at a time, or as a set:
") (text . "") (js . "alert(jQuery.data( document.body, 'foo' ));
	  alert(jQuery.data( document.body ));") (text . "") (text . "The above lines alert the data values that were set on the body
element. If nothing was set on that element, an empty string is
returned.
") (text . "") (text . "Calling jQuery.data(element) retrieves all of the elementʼs associated
values as a JavaScript object. Note that jQuery itself uses this method
to store data for internal use, such as event handlers, so do not
assume that it contains only data that your own code has stored.
") (text . "") (text . "Note: this method currently does not provide cross-platform support for
setting data on XML documents, as Internet Explorer does not allow data
to be attached via expando properties.
") (text . "")) ("examples" ((text . "") (text . "Get the data named \"blah\" stored at for an element.

") (text . "") (js . "
	    $(\"button\").click(function(e) {
	    var value, div = $(\"div\")[0];

	    switch ($(\"button\").index(this)) {
	    case 0 :
	    value = jQuery.data(div, \"blah\");
	    break;
	    case 1 :
	    jQuery.data(div, \"blah\", \"hello\");
	    value = \"Stored!\";
	    break;
	    case 2 :
	    jQuery.data(div, \"blah\", 86);
	    value = \"Stored!\";
	    break;
	    case 3 :
	    jQuery.removeData(div, \"blah\");
	    value = \"Removed!\";
	    break;
	    }

	    $(\"span\").text(\"\" + value);
	    });

	    ") (text . "") (css . "
	    div { margin:5px; background:yellow; }
	    button { margin:5px; font-size:14px; }
	    p { margin:5px; color:blue; }
	    span { color:red; }
	    ") (text . "") (html . "<div>A div</div>
	    <button>Get \"blah\" from the div</button>
	    <button>Set \"blah\" to \"hello\"</button>

	    <button>Set \"blah\" to 86</button>
	    <button>Remove \"blah\" from the div</button>
	    <p>The \"blah\" value of this div is <span>?</span></p>") (text . ""))))) jquery-doc-hash)

(push "$.dequeue" jquery-methods)

(puthash "$.dequeue" (quote (("name" . "$.dequeue") ("signatures" "$.dequeue" (("element" "A DOM element from which to remove and execute a queued function.

" nil nil) ("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" "true" nil))) ("desc" (text . "Execute the next function on the queue for the matched element.

")) ("longdesc" (text . "Note: This is a low-level method, you should probably use .dequeue()
instead.

") (text . "") (text . "When jQuery.dequeue() is called, the next function on the queue is
removed from the queue, and then executed. This function should in turn
(directly or indirectly) cause jQuery.dequeue() to be called, so that
the sequence can continue.
")) ("examples" ((text . "") (text . "Use dequeue to end a custom queue function which allows the queue to
keep going.

") (text . "") (js . "$(\"button\").click(function () {
	    $(\"div\").animate({left:'+=200px'}, 2000);
	    $(\"div\").animate({top:'0px'}, 600);
	    $(\"div\").queue(function () {
	    $(this).toggleClass(\"red\");
	    $.dequeue( this );
	    });
	    $(\"div\").animate({left:'10px', top:'30px'}, 700);
	    });") (text . "") (css . "div { margin:3px; width:50px; position:absolute;
	    height:50px; left:10px; top:30px; 
	    background-color:yellow; }
	    div.red { background-color:red; }  ") (text . "") (html . "<button>Start</button>  <div></div>") (text . ""))))) jquery-doc-hash)

(push "$.queue" jquery-methods)

(puthash "$.queue" (quote (("name" . "$.queue") ("signatures" "$.queue" (("element" "A DOM element to inspect for an attached queue.

" nil nil) ("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" "true" nil))) ("desc" (text . "Show the queue of functions to be executed on the matched element.

")) ("longdesc" (text . "Note: This is a low-level method, you should probably use .queue()
instead.

")) ("examples" ((text . "") (text . "Show the length of the queue.

") (text . "") (js . "$(\"#show\").click(function () {
	    var n = jQuery.queue( $(\"div\")[0], \"fx\" );
	    $(\"span\").text(\"Queue length is: \" + n.length);
	    });
	    function runIt() {
	    $(\"div\").show(\"slow\");
	    $(\"div\").animate({left:'+=200'},2000);
	    $(\"div\").slideToggle(1000);
	    $(\"div\").slideToggle(\"fast\");
	    $(\"div\").animate({left:'-=200'},1500);
	    $(\"div\").hide(\"slow\");
	    $(\"div\").show(1200);
	    $(\"div\").slideUp(\"normal\", runIt);
	    }
	    runIt();") (text . "") (css . "div { margin:3px; width:40px; height:40px;
	    position:absolute; left:0px; top:30px; 
	    background:green; display:none; }
	    div.newcolor { background:blue; }
	    span { color:red; }  ") (text . "") (html . "<button id=\"show\">Show Length of Queue</button>
	    <span></span>
	    <div></div>") (text . ""))))) jquery-doc-hash)

(push "$.queue" jquery-methods)

(puthash "$.queue" (quote (("name" . "$.queue") ("signatures" "$.queue" (("element" "A DOM element where the array of queued functions is attached.

" nil nil) ("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" nil nil) ("newQueue" "An array of functions to replace the current queue contents.

" nil nil)) (("element" "A DOM element on which to add a queued function.

" nil nil) ("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" nil nil) ("callback()" "The new function to add to the queue.

" nil nil))) ("desc" (text . "Manipulate the queue of functions to be executed on the matched
element.

")) ("longdesc" (text . "Note: This is a low-level method, you should probably use .queue()
instead.

") (text . "") (text . "Every element can have one or more queues of functions attached to it
by jQuery. In most applications, only one queue (called fx) is used.
Queues allow a sequence of actions to be called on an element
asynchronously, without halting program execution.
") (text . "") (text . "The jQuery.queue() method allows us to directly manipulate this queue
of functions. Calling jQuery.queue() with a callback is particularly
useful; it allows us to place a new function at the end of the queue.
") (text . "") (text . "Note that when adding a function with jQuery.queue(), we should ensure
that jQuery.dequeue() is eventually called so that the next function in
line executes.
")) ("examples" ((text . "") (text . "Queue a custom function.

") (text . "") (js . "
	    $(document.body).click(function () {
	    $(\"div\").show(\"slow\");
	    $(\"div\").animate({left:'+=200'},2000);
	    jQuery.queue( $(\"div\")[0], \"fx\", function () {
	    $(this).addClass(\"newcolor\");
	    jQuery.dequeue( this );
	    });
	    $(\"div\").animate({left:'-=200'},500);
	    jQuery.queue( $(\"div\")[0], \"fx\", function () {
	    $(this).removeClass(\"newcolor\");
	    jQuery.dequeue( this );
	    });
	    $(\"div\").slideUp();
	    });") (text . "") (css . "
	    div { margin:3px; width:40px; height:40px;
	    position:absolute; left:0px; top:30px; 
	    background:green; display:none; }
	    div.newcolor { background:blue; }
	    ") (text . "") (html . "Click here...
	    <div></div>") (text . "")) ((text . "") (text . "Set a queue array to delete the queue.

") (text . "") (js . "
	    $(\"#start\").click(function () {
	    $(\"div\").show(\"slow\");
	    $(\"div\").animate({left:'+=200'},5000);
	    jQuery.queue( $(\"div\")[0], \"fx\", function () {
	    $(this).addClass(\"newcolor\");
	    jQuery.dequeue( this );
	    });
	    $(\"div\").animate({left:'-=200'},1500);
	    jQuery.queue( $(\"div\")[0], \"fx\", function () {
	    $(this).removeClass(\"newcolor\");
	    jQuery.dequeue( this );
	    });
	    $(\"div\").slideUp();
	    });
	    $(\"#stop\").click(function () {
	    jQuery.queue( $(\"div\")[0], \"fx\", [] );
	    $(\"div\").stop();
	    });
	    ") (text . "") (css . "
	    div { margin:3px; width:40px; height:40px;
	    position:absolute; left:0px; top:30px; 
	    background:green; display:none; }
	    div.newcolor { background:blue; }
	    ") (text . "") (html . "
	    <button id=\"start\">Start</button>
	    <button id=\"stop\">Stop</button>
	    <div></div>") (text . ""))))) jquery-doc-hash)

(push "clearQueue" jquery-methods)

(puthash "clearQueue" (quote (("name" . "clearQueue") ("signatures" "clearQueue" (("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" "true" nil))) ("desc" (text . "Remove from the queue all items that have not yet been run.

")) ("longdesc" (text . "When the .clearQueue() method is called, all functions on the queue
that have not been executed are removed from the queue. When used
without an argument, .clearQueue() removes the remaining functions from
fx, the standard effects queue. In this way it is similar to
.stop(true). However, while the .stop() method is meant to be used only
with animations, .clearQueue() can also be used to remove any function
that has been added to a generic jQuery queue with the .queue() method.
")) ("examples" ((text . "") (text . "Empty the queue.

") (text . "") (js . "$(\"#start\").click(function () {
	    $(\"div\").show(\"slow\");
	    $(\"div\").animate({left:'+=200'},5000);
	    $(\"div\").queue(function () {
	    $(this).addClass(\"newcolor\");
	    $(this).dequeue();
	    });
	    $(\"div\").animate({left:'-=200'},1500);
	    $(\"div\").queue(function () {
	    $(this).removeClass(\"newcolor\");
	    $(this).dequeue();
	    });
	    $(\"div\").slideUp();
	    });
	    $(\"#stop\").click(function () {
	    $(\"div\").clearQueue();
	    $(\"div\").stop();
	    });") (text . "") (css . "
	    div { margin:3px; width:40px; height:40px;
	    position:absolute; left:0px; top:30px; 
	    background:green; display:none; }
	    div.newcolor { background:blue; }
	    ") (text . "") (html . "<button id=\"start\">Start</button>
	    <button id=\"stop\">Stop</button>
	    <div></div>") (text . ""))))) jquery-doc-hash)

(push "toArray" jquery-methods)

(puthash "toArray" (quote (("name" . "toArray") ("signatures" "toArray" nil) ("desc" (text . "Retrieve all the DOM elements contained in the jQuery set, as an array.


")) ("longdesc" (text . ".toArray() returns all of the elements in the jQuery set:


") (text . "") (js . "alert($('li').toArray());") (text . "") (text . "All of the matched DOM nodes are returned by this call, contained in a
standard array:

") (text . "") (text . "[<li id=\"foo\">, <li id=\"bar\">]


")) ("examples" ((text . "") (text . "Selects all divs in the document and returns the DOM Elements as an
Array, then uses the built-in reverse-method to reverse that array.

") (text . "") (js . "
	    
	    function disp(divs) {
	    var a = [];
	    for (var i = 0; i < divs.length; i++) {
	    a.push(divs[i].innerHTML);
	    }
	    $(\"span\").text(a.join(\" \"));
	    }
	    
	    disp( $(\"div\").toArray().reverse() );
	    ") (text . "") (css . "
	    span { color:red; }
	    ") (text . "") (html . "Reversed - <span></span>
	    
	    <div>One</div>
	    <div>Two</div>
	    <div>Three</div>") (text . ""))))) jquery-doc-hash)

(push "$.isEmptyObject" jquery-methods)

(puthash "$.isEmptyObject" (quote (("name" . "$.isEmptyObject") ("signatures" "$.isEmptyObject" (("object" "The object that will be checked to see if itʼs empty.

" nil nil))) ("desc" (text . "Check to see if an object is empty (contains no properties).

")) ("longdesc" (text . "As of jQuery 1.4 this method checks both properties on the object
itself and properties inherited from prototypes (in that it doesnʼt use
hasOwnProperty). The argument should always be a plain JavaScript
Object as other types of object (DOM elements, primitive
strings/numbers, host objects) may not give consistent results across
browsers. To determine if an object is a plain JavaScript object, use
$.isPlainObject()
")) ("examples" ((text . "") (text . "Check an object to see if itʼs empty.

") (text . "") (js . "jQuery.isEmptyObject({}) // true
	    jQuery.isEmptyObject({ foo: \"bar\" }) // false") (text . ""))))) jquery-doc-hash)

(push "$.isPlainObject" jquery-methods)

(puthash "$.isPlainObject" (quote (("name" . "$.isPlainObject") ("signatures" "$.isPlainObject" (("object" "The object that will be checked to see if itʼs a plain object.

" nil nil))) ("desc" (text . "Check to see if an object is a plain object (created using \"{}\" or \"new
Object\").

")) ("longdesc") ("examples" ((text . "") (text . "Check an object to see if itʼs a plain object.

") (text . "") (js . "jQuery.isPlainObject({}) // true
	    jQuery.isPlainObject(\"test\") // false") (text . ""))))) jquery-doc-hash)

(push "keydown" jquery-methods)

(puthash "keydown" (quote (("name" . "keydown") ("signatures" "keydown" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"keydown\" JavaScript event, or trigger
that event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼkeydownʼ, handler) in the first
and second variations, and .trigger(ʼkeydownʼ) in the third.

") (text . "") (text . "The keydown event is sent to an element when the user first presses a
key on the keyboard. It can be attached to any element, but the event
is only sent to the element that has the focus. Focusable elements can
vary between browsers, but form elements can always get focus so are
reasonable candidates for this event type.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<form>
	    <input id=\"target\" type=\"text\" value=\"Hello there\" />
	    </form>
	    <div id=\"other\">
	    Trigger the handler
	    </div>") (text . "") (text . "The event handler can be bound to the input field:


") (text . "") (js . "$('#target').keydown(function() {
	    alert('Handler for .keydown() called.');
	    });") (text . "") (text . "Now when the insertion point is inside the field, pressing a key
displays the alert:

") (text . "") (text . "Handler for .keydown() called.


") (text . "") (text . "To trigger the event manually, apply .keydown() without an argument:


") (text . "") (js . "$('#other').click(function() {
	    $('#target').keydown();
	    });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message.

") (text . "") (text . "If key presses anywhere need to be caught (for example, to implement
global shortcut keys on a page), it is useful to attach this behavior
to the document object. Because of event bubbling, all key presses will
make their way up the DOM to the document object unless explicitly
stopped.
") (text . "") (text . "To determine which key was pressed, examine the event object that is
passed to the handler function. While browsers use differing properties
to store this information, jQuery normalizes the .which property so you
can reliably use it to retrieve the key code. This code corresponds to
a key on the keyboard, including codes for special keys such as arrows.
For catching actual text entry, .keypress() may be a better choice.
") (text . "")) ("examples" ((text . "") (text . "Show the event object for the keydown handler when a key is pressed in
the input.

") (text . "") (js . "
	    var xTriggered = 0;
	    $('#target').keydown(function(event) {
	    if (event.keyCode == '13') {
	    event.preventDefault();
	    }
	    xTriggered++;
	    var msg = 'Handler for .keydown() called ' + xTriggered + ' time(s).';
	    $.print(msg, 'html');
	    $.print(event);
	    });
	    
	    $('#other').click(function() {
	    $('#target').keydown();
	    });") (text . "") (css . "
	    fieldset { margin-bottom: 1em; }
	    input { display: block; margin-bottom: .25em; }
	    #print-output {
	    width: 100%;
	    }
	    .print-output-line {
	    white-space: pre;
	    padding: 5px;
	    font-family: monaco, monospace;
	    font-size: .7em;
	    }
	    
	    ") (text . "") (text . "460

") (text . "") (html . "<form>
	    <fieldset>
	    <label for=\"target\">Type Something:</label>
	    <input id=\"target\" type=\"text\" />
	    </fieldset>
	    </form>
	    <button id=\"other\">
	    Trigger the handler
	    </button>
	    <script type=\"text/javascript\" src=\"/scripts/events.js\"></script>") (text . ""))))) jquery-doc-hash)

(push "index" jquery-methods)

(puthash "index" (quote (("name" . "index") ("signatures" "index" nil (("selector" "A selector representing a jQuery collection in which to look for an
element.

" nil nil)) (("element" "The DOM element or first element within the jQuery object to look for.

" nil nil))) ("desc" (text . "Search for a given element from among the matched elements.

")) ("longdesc" (text . " Return Values


") (text . "") (text . "If no argument is passed to the .index() method, the return value is an
integer indicating the position of the first element within the jQuery
object relative to its sibling elements.
") (text . "") (text . "If .index() is called on a collection of elements and a DOM element or
jQuery object is passed in, .index() returns an integer indicating the
position of the passed element relative to the original collection.
") (text . "") (text . "If a selector string is passed as an argument, .index() returns an
integer indicating the position of the original element relative to the
elements matched by the selector. If the element is not found, .index()
will return -1.
") (text . "") (text . " Detail


") (text . "") (text . "The complementary operation to .get(), which accepts an index and
returns a DOM node, .index() can take a DOM node and returns an index.
Suppose we have a simple unordered list on the page:
") (text . "") (js . "
	    <ul>
	    <li id=\"foo\">foo</li>
	    <li id=\"bar\">bar</li>
	    <li id=\"baz\">baz</li>
	    </ul>
	  ") (text . "") (text . "If we retrieve one of the three list items (for example, through a DOM
function or as the context to an event handler), .index() can search
for this list item within the set of matched elements:
") (text . "") (js . "
	    var listItem = document.getElementById('bar');
	    alert('Index: ' + $('li').index(listItem));
	    We get back the zero-based position of the list item:
	  ") (text . "") (text . "Index: 1


") (text . "") (text . "Similarly, if we retrieve a jQuery object consisting of one of the
three list items, .index() will search for that list item:

") (text . "") (js . "
	    var listItem = $('#bar');
	    alert('Index: ' + $('li').index(listItem));
	  ") (text . "") (text . "We get back the zero-based position of the list item:


") (text . "") (text . "Index: 1


") (text . "") (text . "Note that if the jQuery collection used as the .index() methodʼs
argument contains more than one element, the first element within the
matched set of elements will be used.
") (text . "") (js . "
	    var listItems = $('li:gt(0)');
	    alert('Index: ' + $('li').index(listItems));
	  ") (text . "") (text . "We get back the zero-based position of the first list item within the
matched set:

") (text . "") (text . "Index: 1


") (text . "") (text . "If we use a string as the .index() methodʼs argument, it is interpreted
as a jQuery selector string. The first element among the objectʼs
matched elements which also matches this selector is located.
") (text . "") (js . "
	    var listItem = $('#bar');
	    alert('Index: ' + listItem.index('li'));
	  ") (text . "") (text . "We get back the zero-based position of the list item:


") (text . "") (text . "Index: 1


") (text . "") (text . "If we omit the argument, .index() will return the position of the first
element within the set of matched elements in relation to its siblings:

") (text . "") (js . "alert('Index: ' + $('#bar').index();") (text . "") (text . "Again, we get back the zero-based position of the list item:


") (text . "") (text . "Index: 1


") (text . "")) ("examples" ((text . "") (text . "On click, returns the index (based zero) of that div in the page.

") (text . "") (js . "
	    $(\"div\").click(function () {
	    // this is the dom element clicked
	    var index = $(\"div\").index(this);
	    $(\"span\").text(\"That was div index #\" + index);
	    });
	    ") (text . "") (css . "
	    div { background:yellow; margin:5px; }
	    span { color:red; }
	    ") (text . "") (html . "<span>Click a div!</span>
	    <div>First div</div>
	    <div>Second div</div>
	    <div>Third div</div>") (text . "")) ((text . "") (text . "Returns the index for the element with ID bar.

") (text . "") (css . "div { font-weight: bold; color: #090; }") (text . "") (js . "var listItem = $('#bar');
	    $('div').html( 'Index: ' + $('li').index(listItem) );") (text . "") (html . "<ul>
	    <li id=\"foo\">foo</li>
	    <li id=\"bar\">bar</li>
	    <li id=\"baz\">baz</li>
	    </ul>
	    <div></div>") (text . "")) ((text . "") (text . "Returns the index for the first item in the jQuery collection.

") (text . "") (css . "div { font-weight: bold; color: #090; }") (text . "") (js . "var listItems = $('li:gt(0)');
	    $('div').html( 'Index: ' + $('li').index(listItems) );
	    ") (text . "") (html . "<ul>
	    <li id=\"foo\">foo</li>
	    <li id=\"bar\">bar</li>
	    <li id=\"baz\">baz</li>
	    </ul>
	    <div></div>") (text . "")) ((text . "") (text . "Returns the index for the element with ID bar in relation to all <li>
elements.

") (text . "") (css . "div { font-weight: bold; color: #090; }") (text . "") (js . "$('div').html('Index: ' +  $('#bar').index('li') );") (text . "") (html . "<ul>
	    <li id=\"foo\">foo</li>
	    <li id=\"bar\">bar</li>
	    <li id=\"baz\">baz</li>
	    </ul>
	    <div></div>") (text . "")) ((text . "") (text . "Returns the index for the element with ID bar in relation to its
siblings.

") (text . "") (css . "div { font-weight: bold; color: #090; }") (text . "") (js . "var barIndex = $('#bar').index();
	    $('div').html( 'Index: ' +  barIndex );") (text . "") (html . "<ul>
	    <li id=\"foo\">foo</li>
	    <li id=\"bar\">bar</li>
	    <li id=\"baz\">baz</li>
	    </ul>
	    <div></div>") (text . "")) ((text . "") (text . "Returns -1, as there is no element with ID foobar.

") (text . "") (css . "div { font-weight: bold; color: #090; }") (text . "") (js . "var foobar = $(\"li\").index( $('#foobar') );
	    $('div').html('Index: ' + foobar);") (text . "") (html . "<ul>
	    <li id=\"foo\">foo</li>
	    <li id=\"bar\">bar</li>
	    <li id=\"baz\">baz</li>
	    </ul>
	    <div></div>") (text . ""))))) jquery-doc-hash)

(push "removeData" jquery-methods)

(puthash "removeData" (quote (("name" . "removeData") ("signatures" "removeData" (("name" "A string naming the piece of data to delete.

" "true" nil))) ("desc" (text . "Remove a previously-stored piece of data.

")) ("longdesc" (text . "The .removeData() method allows us to remove values that were
previously set using .data(). When called with the name of a key,
.removeData() deletes that particular value; when called with no
arguments, all values are removed.
") (text . "NOTE: Starting with jQuery 1.4.3, calling .removeData() will cause the
value of the property being removed to revert to the value of the data
attribute of the same name in the DOM, rather than being set to
undefined.
")) ("examples" ((text . "") (text . "Set a data store for 2 names then remove one of them.

") (text . "") (js . "
	    
	    $(\"span:eq(0)\").text(\"\" + $(\"div\").data(\"test1\"));
	    $(\"div\").data(\"test1\", \"VALUE-1\");
	    $(\"div\").data(\"test2\", \"VALUE-2\");
	    $(\"span:eq(1)\").text(\"\" + $(\"div\").data(\"test1\"));
	    $(\"div\").removeData(\"test1\");
	    $(\"span:eq(2)\").text(\"\" + $(\"div\").data(\"test1\"));
	    $(\"span:eq(3)\").text(\"\" + $(\"div\").data(\"test2\"));
	    
	    ") (text . "") (css . "
	    div { margin:2px; color:blue; }
	    span { color:red; }
	    ") (text . "") (html . "<div>value1 before creation: <span></span></div>
	    <div>value1 after creation: <span></span></div>
	    <div>value1 after removal: <span></span></div>
	    
	    <div>value2 after removal: <span></span></div>") (text . ""))))) jquery-doc-hash)

(push "data" jquery-methods)

(puthash "data" (quote (("name" . "data") ("signatures" "data" (("key" "A string naming the piece of data to set.

" nil nil) ("value" "The new data value; it can be any Javascript type including Array or
Object.

" nil nil)) (("obj" "An object of key-value pairs of data to update.

" nil nil))) ("desc" (text . "Store arbitrary data associated with the matched elements.

")) ("longdesc" (text . "The .data() method allows us to attach data of any type to DOM elements
in a way that is safe from circular references and therefore from
memory leaks.
") (text . "") (text . " We can set several distinct values for a single element and retrieve
them later:

") (text . "") (js . "
	    $('body').data('foo', 52);
	    $('body').data('bar', { myType: 'test', count: 40 });
	    
	    $('body').data('foo'); // 52
	    $('body').data(); // {foo: 52, bar: { myType: 'test', count: 40 }}
	  ") (text . "") (text . "In jQuery 1.4.3 setting an elementʼs data object with .data(obj)
extends the data previously stored with that element. jQuery itself
uses the .data() method to save information under the names ʼeventsʼ
and ʼhandleʼ, and also reserves any data name starting with an
underscore (ʼ_ʼ) for internal use.
") (text . "") (text . "Prior to jQuery 1.4.3 (starting in jQuery 1.4) the .data() method
completely replaced all data, instead of just extending the data
object. If you are using third-party plugins it may not be advisable to
completely replace the elementʼs data object, since plugins may have
also set data.
") (text . "") (text . "Due to the way browsers interact with plugins and external code, the
.data() method cannot be used on <object> (unless itʼs a Flash plugin),
<applet> or <embed> elements.
") (text . "")) ("examples" ((text . "") (text . "Store then retrieve a value from the div element.

") (text . "") (js . "
	    $(\"div\").data(\"test\", { first: 16, last: \"pizza!\" });
	    $(\"span:first\").text($(\"div\").data(\"test\").first);
	    $(\"span:last\").text($(\"div\").data(\"test\").last);
	    ") (text . "") (css . "
	    div { color:blue; }
	    span { color:red; }
	    ") (text . "") (html . "<div>
	    The values stored were 
	    <span></span>
	    and
	    <span></span>
	    </div>") (text . ""))))) jquery-doc-hash)

(push "data" jquery-methods)

(puthash "data" (quote (("name" . "data") ("signatures" "data" (("key" "Name of the data stored.

" nil nil)) nil) ("desc" (text . "Returns value at named data store for the first element in the jQuery
collection, as set by data(name, value).

")) ("longdesc" (text . "") (text . "The .data() method allows us to attach data of any type to DOM elements
in a way that is safe from circular references and therefore from
memory leaks. We can retrieve several distinct values for a single
element one at a time, or as a set:
") (text . "") (js . "
	      alert($('body').data('foo'));
	      alert($('body').data());
	    ") (text . "") (text . "The above lines alert the data values that were set on the body
element. If no data at all was set on that element, undefined is
returned.
") (text . "") (js . "
	      alert( $(\"body\").data(\"foo\")); //undefined
	      $(\"body\").data(\"bar\", \"foobar\");
	      alert( $(\"body\").data(\"foobar\")); //foobar
	    ") (text . "") (text . "HTML 5 data- Attributes


") (text . "") (text . "As of jQuery 1.4.3 HTML 5 data- attributes will be automatically pulled
in to jQueryʼs data object. The treatment of attributes with embedded
dashes was changed in jQuery 1.6 to conform to the W3C HTML5
specification.
") (text . "") (text . "For example, given the following HTML:


") (text . "") (js . "<div data-role=\"page\" data-last-value=\"43\" data-hidden=\"true\" data-options='{\"name\":\"John\"}'></div>") (text . "") (text . "All of the following jQuery code will work.


") (text . "") (js . "$(\"div\").data(\"role\") === \"page\";
	    $(\"div\").data(\"lastValue\") === 43;
	    $(\"div\").data(\"hidden\") === true;
	    $(\"div\").data(\"options\").name === \"John\";") (text . "") (text . "Every attempt is made to convert the string to a JavaScript value (this
includes booleans, numbers, objects, arrays, and null) otherwise it is
left as a string. To retrieve the valueʼs attribute as a string without
any attempt to convert it, use the attr() method. When the data
attribute is an object (starts with ʼ{ʼ) or array (starts with ʼ[ʼ)
then jQuery.parseJSON is used to parse the string; it must follow valid
JSON syntax including quoted property names. The data- attributes are
pulled in the first time the data property is accessed and then are no
longer accessed or mutated (all data values are then stored internally
in jQuery).
") (text . "") (text . "Calling .data() with no parameters retrieves all of the values as a
JavaScript object. This object can be safely cached in a variable as
long as a new object is not set with .data(obj). Using the object
directly to get or set values is faster than making individual calls to
.data() to get or set each value:
") (text . "") (js . "
	      var mydata = $(\"#mydiv\").data();
	      if ( mydata.count < 9 ) {
	      mydata.count = 43;
	      mydata.status = \"embiggened\";
	      }
	    ") (text . "")) ("examples" ((text . "") (text . "Get the data named \"blah\" stored at for an element.

") (text . "") (js . "
	    $(\"button\").click(function(e) {
	    var value;
	    
	    switch ($(\"button\").index(this)) {
	    case 0 :
	    value = $(\"div\").data(\"blah\");
	    break;
	    case 1 :
	    $(\"div\").data(\"blah\", \"hello\");
	    value = \"Stored!\";
	    break;
	    case 2 :
	    $(\"div\").data(\"blah\", 86);
	    value = \"Stored!\";
	    break;
	    case 3 :
	    $(\"div\").removeData(\"blah\");
	    value = \"Removed!\";
	    break;
	    }
	    
	    $(\"span\").text(\"\" + value);
	    });
	    
	    ") (text . "") (css . "
	    div { margin:5px; background:yellow; }
	    button { margin:5px; font-size:14px; }
	    p { margin:5px; color:blue; }
	    span { color:red; }
	    ") (text . "") (html . "<div>A div</div>
	    <button>Get \"blah\" from the div</button>
	    <button>Set \"blah\" to \"hello\"</button>
	    
	    <button>Set \"blah\" to 86</button>
	    <button>Remove \"blah\" from the div</button>
	    <p>The \"blah\" value of this div is <span>?</span></p>") (text . ""))))) jquery-doc-hash)

(push "get" jquery-methods)

(puthash "get" (quote (("name" . "get") ("signatures" "get" (("index" "A zero-based integer indicating which element to retrieve.

" "true" nil))) ("desc" (text . "Retrieve the DOM elements matched by the jQuery object.

")) ("longdesc" (text . "The .get() method grants us access to the DOM nodes underlying each
jQuery object. Suppose we had a simple unordered list on the page:

") (text . "") (js . "
	      <ul>
	      <li id=\"foo\">foo</li>
	      <li id=\"bar\">bar</li>
	      </ul>
	    ") (text . "") (text . "Without a parameter, .get() returns all of the elements:


") (text . "") (js . "alert($('li').get());") (text . "") (text . "All of the matched DOM nodes are returned by this call, contained in a
standard array:

") (text . "") (text . "[<li id=\"foo\">, <li id=\"bar\">]


") (text . "") (text . "With an index specified, .get() will retrieve a single element:


") (text . "") (js . "($('li').get(0));") (text . "") (text . "Since the index is zero-based, the first list item is returned:


") (text . "") (text . "<li id=\"foo\">


") (text . "") (text . "Each jQuery object also masquerades as an array, so we can use the
array dereferencing operator to get at the list item instead:

") (text . "") (js . "alert($('li')[0]);") (text . "") (text . "However, this syntax lacks some of the additional capabilities of
.get(), such as specifying a negative index:

") (text . "") (js . "alert($('li').get(-1));") (text . "") (text . "A negative index is counted from the end of the matched set, so this
example will return the last item in the list:

") (text . "") (text . "<li id=\"bar\">


")) ("examples" ((text . "") (text . "Selects all divs in the document and returns the DOM Elements as an
Array, then uses the built-in reverse-method to reverse that array.

") (text . "") (js . "
	      
	      function disp(divs) {
	      var a = [];
	      for (var i = 0; i < divs.length; i++) {
	      a.push(divs[i].innerHTML);
	      }
	      $(\"span\").text(a.join(\" \"));
	      }
	      
	      disp( $(\"div\").get().reverse() );
	      ") (text . "") (css . "
	      span { color:red; }
	      ") (text . "") (html . "Reversed - <span></span>
	      
	      <div>One</div>
	      <div>Two</div>
	      <div>Three</div>") (text . "")) ((text . "") (text . "Gives the tag name of the element clicked on.

") (text . "") (js . "
	      
	      $(\"*\", document.body).click(function (e) {
	      e.stopPropagation();
	      var domEl = $(this).get(0);
	      $(\"span:first\").text(\"Clicked on - \" + domEl.tagName);
	      });
	      ") (text . "") (css . "
	      span { color:red; }
	      div { background:yellow; }
	      ") (text . "") (html . "<span>&nbsp;</span>
	      <p>In this paragraph is an <span>important</span> section</p>
	      
	      <div><input type=\"text\" /></div>") (text . ""))))) jquery-doc-hash)

(push "size" jquery-methods)

(puthash "size" (quote (("name" . "size") ("signatures" "size" nil) ("desc" (text . "Return the number of elements in the jQuery object.

")) ("longdesc" (text . "") (text . "The .size() method is functionally equivalent to the .length property;
however, the .length property is preferred because it does not have the
overhead of a function call.
") (text . "") (text . "Given a simple unordered list on the page:


") (text . "") (js . "
		<ul>
		<li>foo</li>
		<li>bar</li>
		</ul>
	      ") (text . "") (text . "Both .size() and .length identify the number of items:


") (text . "") (js . "alert( \"Size: \" + $(\"li\").size() );
	      alert( \"Size: \" + $(\"li\").length );") (text . "") (text . "This results in two alerts:


") (text . "") (text . "Size: 2


") (text . "") (text . "Size: 2


") (text . "")) ("examples" ((text . "") (text . "Count the divs. Click to add more.

") (text . "") (js . "
	      $(document.body)
	      .click(function() { 
	      $(this).append( $(\"<div>\") );
	      var n = $(\"div\").size();
	      $(\"span\").text(\"There are \" + n + \" divs. Click to add more.\");
	      })
	      // trigger the click to start
	      .click(); 
	      ") (text . "") (css . "
	      body { cursor:pointer; min-height: 100px; }
	      div { width:50px; height:30px; margin:5px; 
	      float:left; background:blue; }
	      span { color:red; }
	      ") (text . "") (html . "
	      <span></span>
	      <div></div>
	      ") (text . ""))))) jquery-doc-hash)

(push "$.noConflict" jquery-methods)

(puthash "$.noConflict" (quote (("name" . "$.noConflict") ("signatures" "$.noConflict" (("removeAll" "A Boolean indicating whether to remove all jQuery variables from the
global scope (including jQuery itself).

" "true" nil))) ("desc" (text . "Relinquish jQueryʼs control of the $ variable.

")) ("longdesc" (text . "Many JavaScript libraries use $ as a function or variable name, just as
jQuery does. In jQueryʼs case, $ is just an alias for jQuery, so all
functionality is available without using $. If we need to use another
JavaScript library alongside jQuery, we can return control of $ back to
the other library with a call to $.noConflict():
") (text . "") (js . "
	      <script type=\"text/javascript\" src=\"other_lib.js\"></script>
	      <script type=\"text/javascript\" src=\"jquery.js\"></script>
	      <script type=\"text/javascript\">
	      $.noConflict();
	      // Code that uses other library's $ can follow here.
	      </script>
	    ") (text . "") (text . "This technique is especially effective in conjunction with the .ready()
methodʼs ability to alias the jQuery object, as within callback passed
to .ready() we can use $ if we wish without fear of conflicts later:
") (text . "") (js . "
	      <script type=\"text/javascript\" src=\"other_lib.js\"></script>
	      <script type=\"text/javascript\" src=\"jquery.js\"></script>
	      <script type=\"text/javascript\">
	      $.noConflict();
	      jQuery(document).ready(function($) {
	      // Code that uses jQuery's $ can follow here.
	      });
	      // Code that uses other library's $ can follow here.
	      </script>
	    ") (text . "") (text . "If necessary, we can free up the jQuery name as well by passing true as
an argument to the method. This is rarely necessary, and if we must do
this (for example, if we need to use multiple versions of the jQuery
library on the same page), we need to consider that most plug-ins rely
on the presence of the jQuery variable and may not operate correctly in
this situation.
") (text . "")) ("examples" ((text . "") (text . "Maps the original object that was referenced by $ back to $.

") (text . "") (js . "jQuery.noConflict();
	      // Do something with jQuery
	      jQuery(\"div p\").hide();
	      // Do something with another library's $()
	      $(\"content\").style.display = 'none';") (text . "")) ((text . "") (text . "Reverts the $ alias and then creates and executes a function to provide
the $ as a jQuery alias inside the functions scope. Inside the function
the original $ object is not available. This works well for most
plugins that donʼt rely on any other library.
") (text . "") (js . "jQuery.noConflict();
	      (function($) { 
	      $(function() {
	      // more code using $ as alias to jQuery
	      });
	      })(jQuery);
	      // other code using $ as an alias to the other library") (text . "")) ((text . "") (text . "You can chain the jQuery.noConflict() with the shorthand ready for a
compact code.

") (text . "") (js . "jQuery.noConflict()(function(){
	      // code using jQuery
	      }); 
	      // other code using $ as an alias to the other library") (text . "")) ((text . "") (text . "Creates a different alias instead of jQuery to use in the rest of the
script.

") (text . "") (js . "var j = jQuery.noConflict();
	      // Do something with jQuery
	      j(\"div p\").hide();
	      // Do something with another library's $()
	      $(\"content\").style.display = 'none';") (text . "")) ((text . "") (text . "Completely move jQuery to a new namespace in another object.

") (text . "") (js . "var dom = {};
	      dom.query = jQuery.noConflict(true);") (text . "") (text . "// Do something with the new jQuery dom.query(\"div p\").hide(); // Do
something with another libraryʼs $() $(\"content\").style.display =
ʼnoneʼ; // Do something with another version of jQuery jQuery(\"div >
p\").hide();
") (text . ""))))) jquery-doc-hash)

(push "scroll" jquery-methods)

(puthash "scroll" (quote (("name" . "scroll") ("signatures" "scroll" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"scroll\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼscrollʼ, handler) in the first and
second variations, and .trigger(ʼscrollʼ) in the third.

") (text . "") (text . "The scroll event is sent to an element when the user scrolls to a
different place in the element. It applies to window objects, but also
to scrollable frames and elements with the overflow CSS property set to
scroll (or auto when the elementʼs explicit height or width is less
than the height or width of its contents).
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<div id=\"target\" style=\"overflow: scroll; width: 200px; height: 100px;\">
	      Lorem ipsum dolor sit amet, consectetur adipisicing elit,
	      sed do eiusmod tempor incididunt ut labore et dolore magna
	      aliqua. Ut enim ad minim veniam, quis nostrud exercitation
	      ullamco laboris nisi ut aliquip ex ea commodo consequat.
	      Duis aute irure dolor in reprehenderit in voluptate velit
	      esse cillum dolore eu fugiat nulla pariatur. Excepteur
	      sint occaecat cupidatat non proident, sunt in culpa qui
	      officia deserunt mollit anim id est laborum.
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      <div id=\"log\"></div>") (text . "") (text . "The style definition is present to make the target element small enough
to be scrollable:

") (text . "") (text . "

") (text . "") (text . "The scroll event handler can be bound to this element:


") (text . "") (js . "$('#target').scroll(function() {
	      $('#log').append('<div>Handler for .scroll() called.</div>');
	      });") (text . "") (text . "Now when the user scrolls the text up or down, one or more messages are
appended to <div id=\"log\"></div>:

") (text . "") (text . "Handler for .scroll() called.


") (text . "") (text . "To trigger the event manually, apply .scroll() without an argument:


") (text . "") (js . "$('#other').click(function() {
	      $('#target').scroll();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also
append the message.

") (text . "") (text . "A scroll event is sent whenever the elementʼs scroll position changes,
regardless of the cause. A mouse click or drag on the scroll bar,
dragging inside the element, pressing the arrow keys, or using the
mouseʼs scroll wheel could cause this event.
") (text . "")) ("examples" ((text . "") (text . "To do something when your page is scrolled:

") (text . "") (js . "
	      $(\"p\").clone().appendTo(document.body);
	      $(\"p\").clone().appendTo(document.body);
	      $(\"p\").clone().appendTo(document.body);
	      $(window).scroll(function () { 
	      $(\"span\").css(\"display\", \"inline\").fadeOut(\"slow\"); 
	      });

	      ") (text . "") (css . "
	      div { color:blue; }
	      p { color:green; }
	      span { color:red; display:none; }
	      ") (text . "") (html . "<div>Try scrolling the iframe.</div>
	      <p>Paragraph - <span>Scroll happened!</span></p>") (text . ""))))) jquery-doc-hash)

(push "resize" jquery-methods)

(puthash "resize" (quote (("name" . "resize") ("signatures" "resize" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"resize\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼresizeʼ, handler) in the first and
second variations, and .trigger(ʼresizeʼ) in the third.

") (text . "") (text . "The resize event is sent to the window element when the size of the
browser window changes:

") (text . "") (js . "$(window).resize(function() {
	      $('#log').append('<div>Handler for .resize() called.</div>');
	      });
	      ") (text . "") (text . "Now whenever the browser windowʼs size is changed, the message is
appended to <div id=\"log\"> one or more times, depending on the browser.

") (text . "") (text . "Code in a resize handler should never rely on the number of times the
handler is called. Depending on implementation, resize events can be
sent continuously as the resizing is in progress (the typical behavior
in Internet Explorer and WebKit-based browsers such as Safari and
Chrome), or only once at the end of the resize operation (the typical
behavior in Firefox).
") (text . "")) ("examples" ((text . "") (text . "To see the window width while (or after) it is resized, try:

") (text . "") (js . "
	      $(window).resize(function() {
	      $('body').prepend('<div>' + $(window).width() + '</div>');
	      });
	      ") (text . ""))))) jquery-doc-hash)

(push "dequeue" jquery-methods)

(puthash "dequeue" (quote (("name" . "dequeue") ("signatures" "dequeue" (("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" "true" nil))) ("desc" (text . "Execute the next function on the queue for the matched elements.

")) ("longdesc" (text . "When .dequeue() is called, the next function on the queue is removed
from the queue, and then executed. This function should in turn
(directly or indirectly) cause .dequeue() to be called, so that the
sequence can continue.
")) ("examples" ((text . "") (text . "Use dequeue to end a custom queue function which allows the queue to
keep going.

") (text . "") (js . "
	      $(\"button\").click(function () {
	      $(\"div\").animate({left:'+=200px'}, 2000);
	      $(\"div\").animate({top:'0px'}, 600);
	      $(\"div\").queue(function () {
	      $(this).toggleClass(\"red\");
	      $(this).dequeue();
	      });
	      $(\"div\").animate({left:'10px', top:'30px'}, 700);
	      });
	      ") (text . "") (css . "
	      div { margin:3px; width:50px; position:absolute;
	      height:50px; left:10px; top:30px; 
	      background-color:yellow; }
	      div.red { background-color:red; }  
	      ") (text . "") (html . "<button>Start</button>  
	      <div></div>") (text . ""))))) jquery-doc-hash)

(push "queue" jquery-methods)

(puthash "queue" (quote (("name" . "queue") ("signatures" "queue" (("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" "true" nil))) ("desc" (text . "Show the queue of functions to be executed on the matched elements.

")) ("longdesc") ("examples" ((text . "") (text . "Show the length of the queue.

") (text . "") (js . "$(\"#show\").click(function () {
	      var n = $(\"div\").queue(\"fx\");
	      $(\"p\").text(\"Queue length is: \" + n.length);
	      });
	      function runIt() {
	      $(\"div\").show(\"slow\");
	      $(\"div\").animate({left:'+=200'},2000);
	      $(\"div\").slideToggle(1000);
	      $(\"div\").slideToggle(\"fast\");
	      $(\"div\").animate({left:'-=200'},1500);
	      $(\"div\").hide(\"slow\");
	      $(\"div\").show(1200);
	      $(\"div\").slideUp(\"normal\", runIt);
	      }
	      runIt();") (text . "") (css . "div { margin:3px; width:40px; height:40px;
	      position:absolute; left:0px; top:60px; 
	      background:green; display:none; }
	      div.newcolor { background:blue; }
	      p { color:red; }  ") (text . "") (html . "<button id=\"show\">Show Length of Queue</button>
	      <p></p>
	      <div></div>") (text . ""))))) jquery-doc-hash)

(push "queue" jquery-methods)

(puthash "queue" (quote (("name" . "queue") ("signatures" "queue" (("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" "true" nil) ("newQueue" "An array of functions to replace the current queue contents.

" nil nil)) (("queueName" "A string containing the name of the queue. Defaults to fx, the standard
effects queue.

" "true" nil) ("callback( next )" "The new function to add to the queue, with a function to call that will
dequeue the next item.

" nil nil))) ("desc" (text . "Manipulate the queue of functions to be executed on the matched
elements.

")) ("longdesc" (text . "Every element can have one to many queues of functions attached to it
by jQuery. In most applications, only one queue (called fx) is used.
Queues allow a sequence of actions to be called on an element
asynchronously, without halting program execution. The typical example
of this is calling multiple animation methods on an element. For
example:
") (text . "") (js . "$('#foo').slideUp().fadeIn();") (text . "") (text . "When this statement is executed, the element begins its sliding
animation immediately, but the fading transition is placed on the fx
queue to be called only once the sliding transition is complete.
") (text . "") (text . "The .queue() method allows us to directly manipulate this queue of
functions. Calling .queue() with a callback is particularly useful; it
allows us to place a new function at the end of the queue.
") (text . "") (text . "This feature is similar to providing a callback function with an
animation method, but does not require the callback to be given at the
time the animation is performed.
") (text . "") (js . "$('#foo').slideUp();
	    $('#foo').queue(function() {
	    alert('Animation complete.');
	    $(this).dequeue();
	    });") (text . "") (text . "This is equivalent to:


") (text . "") (js . "$('#foo').slideUp(function() {
	    alert('Animation complete.');
	    });") (text . "") (text . "Note that when adding a function with .queue(), we should ensure that
.dequeue() is eventually called so that the next function in line
executes.
") (text . "") (text . "In jQuery 1.4 the function thatʼs called is passed in another function,
as the first argument, that when called automatically dequeues the next
item and keeps the queue moving. You would use it like so:
") (text . "") (js . "$(\"#test\").queue(function(next) {
	    // Do some stuff...
	    next();
	    });")) ("examples" ((text . "") (text . "Queue a custom function.

") (text . "") (js . "$(document.body).click(function () {
	      $(\"div\").show(\"slow\");
	      $(\"div\").animate({left:'+=200'},2000);
	      $(\"div\").queue(function () {
	      $(this).addClass(\"newcolor\");
	      $(this).dequeue();
	      });
	      $(\"div\").animate({left:'-=200'},500);
	      $(\"div\").queue(function () {
	      $(this).removeClass(\"newcolor\");
	      $(this).dequeue();
	      });
	      $(\"div\").slideUp();
	      });") (text . "") (css . "
	      div { margin:3px; width:40px; height:40px;
	      position:absolute; left:0px; top:30px; 
	      background:green; display:none; }
	      div.newcolor { background:blue; }
	      ") (text . "") (html . "Click here...
	      <div></div>") (text . "")) ((text . "") (text . "Set a queue array to delete the queue.

") (text . "") (js . "$(\"#start\").click(function () {
	      $(\"div\").show(\"slow\");
	      $(\"div\").animate({left:'+=200'},5000);
	      $(\"div\").queue(function () {
	      $(this).addClass(\"newcolor\");
	      $(this).dequeue();
	      });
	      $(\"div\").animate({left:'-=200'},1500);
	      $(\"div\").queue(function () {
	      $(this).removeClass(\"newcolor\");
	      $(this).dequeue();
	      });
	      $(\"div\").slideUp();
	      });
	      $(\"#stop\").click(function () {
	      $(\"div\").queue(\"fx\", []);
	      $(\"div\").stop();
	      });") (text . "") (css . "
	      div { margin:3px; width:40px; height:40px;
	      position:absolute; left:0px; top:30px; 
	      background:green; display:none; }
	      div.newcolor { background:blue; }
	      ") (text . "") (html . "<button id=\"start\">Start</button>
	      <button id=\"stop\">Stop</button>
	      <div></div>") (text . ""))))) jquery-doc-hash)

(push "keyup" jquery-methods)

(puthash "keyup" (quote (("name" . "keyup") ("signatures" "keyup" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"keyup\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼkeyupʼ, handler) in the first two
variations, and .trigger(ʼkeyupʼ) in the third.

") (text . "") (text . "The keyup event is sent to an element when the user releases a key on
the keyboard. It can be attached to any element, but the event is only
sent to the element that has the focus. Focusable elements can vary
between browsers, but form elements can always get focus so are
reasonable candidates for this event type.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<form>
	      <input id=\"target\" type=\"text\" value=\"Hello there\" />
	      </form>
	      <div id=\"other\">
	      Trigger the handler
	      </div>") (text . "") (text . "The event handler can be bound to the input field:


") (text . "") (js . "$('#target').keyup(function() {
	      alert('Handler for .keyup() called.');
	      });
	      ") (text . "") (text . "Now when the insertion point is inside the field and a key is pressed
and released, the alert is displayed:

") (text . "") (text . "Handler for .keyup() called.


") (text . "") (text . "To trigger the event manually, apply .keyup() without arguments:


") (text . "") (js . "$('#other').click(function() {
	      $('#target').keyup();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message.

") (text . "") (text . "If key presses anywhere need to be caught (for example, to implement
global shortcut keys on a page), it is useful to attach this behavior
to the document object. Because of event bubbling, all key presses will
make their way up the DOM to the document object unless explicitly
stopped.
") (text . "") (text . "To determine which key was pressed, examine the event object that is
passed to the handler function. While browsers use differing properties
to store this information, jQuery normalizes the .which property so you
can reliably use it to retrieve the key code. This code corresponds to
a key on the keyboard, including codes for special keys such as arrows.
For catching actual text entry, .keypress() may be a better choice.
") (text . "")) ("examples" ((text . "") (text . "Show the event object for the keyup handler when a key is released in
the input.

") (text . "") (js . "
	      var xTriggered = 0;
	      $('#target').keyup(function(event) {
	      if (event.keyCode == '13') {
	      event.preventDefault();
	      }
	      xTriggered++;
	      var msg = 'Handler for .keyup() called ' + xTriggered + ' time(s).';
	      $.print(msg, 'html');
	      $.print(event);
	      });

	      $('#other').click(function() {
	      $('#target').keyup();
	      });") (text . "") (css . "
	      fieldset { margin-bottom: 1em; }
	      input { display: block; margin-bottom: .25em; }
	      #print-output {
	      width: 100%;
	      }
	      .print-output-line {
	      white-space: pre;
	      padding: 5px;
	      font-family: monaco, monospace;
	      font-size: .7em;
	      }

	      ") (text . "") (text . "460

") (text . "") (html . "<form>
	      <fieldset>
	      <label for=\"target\">Type Something:</label>
	      <input id=\"target\" type=\"text\" />
	      </fieldset>
	      </form>
	      <button id=\"other\">
	      Trigger the handler
	      </button>
	      <script type=\"text/javascript\" src=\"/scripts/events.js\"></script>") (text . ""))))) jquery-doc-hash)

(push "keypress" jquery-methods)

(puthash "keypress" (quote (("name" . "keypress") ("signatures" "keypress" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"keypress\" JavaScript event, or trigger
that event on an element.

")) ("longdesc" (text . "") (text . "Note: as the keypress event isnʼt covered by any official
specification, the actual behavior encountered when using it may differ
across browsers, browser versions, and platforms.
") (text . "") (text . "This method is a shortcut for .bind(ʼkeypressʼ, handler) in the first
two variations, and .trigger(ʼkeypressʼ) in the third.

") (text . "") (text . "The keypress event is sent to an element when the browser registers
keyboard input. This is similar to the keydown event, except in the
case of key repeats. If the user presses and holds a key, a keydown
event is triggered once, but separate keypress events are triggered for
each inserted character. In addition, modifier keys (such as Shift)
trigger keydown events but not keypress events.
") (text . "") (text . "A keypress event handler can be attached to any element, but the event
is only sent to the element that has the focus. Focusable elements can
vary between browsers, but form elements can always get focus so are
reasonable candidates for this event type.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<form>
	      <fieldset>
	      <input id=\"target\" type=\"text\" value=\"Hello there\" />
	      </fieldset>
	      </form>
	      <div id=\"other\">
	      Trigger the handler
	      </div>") (text . "") (text . "The event handler can be bound to the input field:


") (text . "") (js . "$('#target').keypress(function() {
	      alert('Handler for .keypress() called.');
	      });") (text . "") (text . "Now when the insertion point is inside the field, pressing a key
displays the alert:

") (text . "") (text . "Handler for .keypress() called.


") (text . "") (text . "The message repeats if the key is held down. To trigger the event
manually, apply .keypress() without an argument::

") (text . "") (js . "$('#other').click(function() {
	      $('#target').keypress();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message.

") (text . "") (text . "If key presses anywhere need to be caught (for example, to implement
global shortcut keys on a page), it is useful to attach this behavior
to the document object. Because of event bubbling, all key presses will
make their way up the DOM to the document object unless explicitly
stopped.
") (text . "") (text . "To determine which character was entered, examine the event object that
is passed to the handler function. While browsers use differing
properties to store this information, jQuery normalizes the .which
property so you can reliably use it to retrieve the character code.
") (text . "") (text . "Note that keydown and keyup provide a code indicating which key is
pressed, while keypress indicates which character was entered. For
example, a lowercase \"a\" will be reported as 65 by keydown and keyup,
but as 97 by keypress. An uppercase \"A\" is reported as 65 by all
events. Because of this distinction, when catching special keystrokes
such as arrow keys, .keydown() or .keyup() is a better choice.
") (text . "")) ("examples" ((text . "") (text . "Show the event object for the keypress handler when a key is pressed in
the input.

") (text . "") (js . "
	      var xTriggered = 0;
	      $('#target').keypress(function(event) {
	      if (event.which == '13') {
	      event.preventDefault();
	      }
	      xTriggered++;
	      var msg = 'Handler for .keypress() called ' + xTriggered + ' time(s).';
	      $.print(msg, 'html');
	      $.print(event);
	      });
	      
	      $('#other').click(function() {
	      $('#target').keypress();
	      });") (text . "") (css . "
	      fieldset { margin-bottom: 1em; }
	      input { display: block; margin-bottom: .25em; }
	      #print-output {
	      width: 100%;
	      }
	      .print-output-line {
	      white-space: pre;
	      padding: 5px;
	      font-family: monaco, monospace;
	      font-size: .7em;
	      }
	      
	      ") (text . "") (text . "460

") (text . "") (html . "<form>
	      <fieldset>
	      <label for=\"target\">Type Something:</label>
	      <input id=\"target\" type=\"text\" />
	      </fieldset>
	      </form>
	      <button id=\"other\">
	      Trigger the handler
	      </button>
	      <script type=\"text/javascript\" src=\"/scripts/events.js\"></script>") (text . ""))))) jquery-doc-hash)

(push "submit" jquery-methods)

(puthash "submit" (quote (("name" . "submit") ("signatures" "submit" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"submit\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼsubmitʼ, handler) in the first
variation, and .trigger(ʼsubmitʼ) in the third.

") (text . "") (text . "The submit event is sent to an element when the user is attempting to
submit a form. It can only be attached to <form> elements. Forms can be
submitted either by clicking an explicit <input type=\"submit\">, <input
type=\"image\">, or <button type=\"submit\">, or by pressing Enter when
certain form elements have focus.
") (text . "") (text . "  Depending on the browser, the Enter key may only cause a form
  submission if the form has exactly one text field, or only when
  there is a submit button present. The interface should not rely on a
  particular behavior for this key unless the issue is forced by
  observing the keypress event for presses of the Enter key.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<form id=\"target\" action=\"destination.html\">
	      <input type=\"text\" value=\"Hello there\" />
	      <input type=\"submit\" value=\"Go\" />
	      </form>
	      <div id=\"other\">
	      Trigger the handler
	      </div>") (text . "") (text . "The event handler can be bound to the form:


") (text . "") (js . "$('#target').submit(function() {
	      alert('Handler for .submit() called.');
	      return false;
	      });") (text . "") (text . "Now when the form is submitted, the message is alerted. This happens
prior to the actual submission, so we can cancel the submit action by
calling .preventDefault() on the event object or by returning false
from our handler. We can trigger the event manually when another
element is clicked:
") (text . "") (js . "$('#other').click(function() {
	      $('#target').submit();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also
display the message. In addition, the default submit action on the form
will be fired, so the form will be submitted.
") (text . "") (text . "The JavaScript submit event does not bubble in Internet Explorer.
However, scripts that rely on event delegation with the submit event
will work consistently across browsers as of jQuery 1.4, which has
normalized the eventʼs behavior.
") (text . "")) ("examples" ((text . "") (text . "If youʼd like to prevent forms from being submitted unless a flag
variable is set, try:

") (text . "") (js . "
	      
	      $(\"form\").submit(function() {
	      if ($(\"input:first\").val() == \"correct\") {
	      $(\"span\").text(\"Validated...\").show();
	      return true;
	      }
	      $(\"span\").text(\"Not valid!\").show().fadeOut(1000);
	      return false;
	      });
	      ") (text . "") (css . "
	      
	      p { margin:0; color:blue; }
	      div,p { margin-left:10px; }
	      span { color:red; }
	      ") (text . "") (html . "<p>Type 'correct' to validate.</p>
	      <form action=\"javascript:alert('success!');\">
	      <div>
	      <input type=\"text\" />
	      
	      <input type=\"submit\" />
	      </div>
	      </form>
	      <span></span>") (text . "")) ((text . "") (text . "If youʼd like to prevent forms from being submitted unless a flag
variable is set, try:

") (text . "") (js . "$(\"form\").submit( function () {
	      return this.some_flag_variable;
	      } );") (text . "")) ((text . "") (text . "To trigger the submit event on the first form on the page, try:

") (text . "") (js . "$(\"form:first\").submit();") (text . ""))))) jquery-doc-hash)

(push "select" jquery-methods)

(puthash "select" (quote (("name" . "select") ("signatures" "select" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"select\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼselectʼ, handler) in the first two
variations, and .trigger(ʼselectʼ) in the third.

") (text . "") (text . "The select event is sent to an element when the user makes a text
selection inside it. This event is limited to <input type=\"text\">
fields and <textarea> boxes.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<form>
	      <input id=\"target\" type=\"text\" value=\"Hello there\" />
	      </form>
	      <div id=\"other\">
	      Trigger the handler
	      </div>") (text . "") (text . "The event handler can be bound to the text input:


") (text . "") (js . "$('#target').select(function() {
	      alert('Handler for .select() called.');
	      });") (text . "") (text . "Now when any portion of the text is selected, the alert is displayed.
Merely setting the location of the insertion point will not trigger the
event. To trigger the event manually, apply .select() without an
argument:
") (text . "") (js . "$('#other').click(function() {
	      $('#target').select();
	      });") (text . "") (text . "After this code executes, clicks on the Trigger button will also alert
the message:

") (text . "") (text . "Handler for .select() called.


") (text . "") (text . "In addition, the default select action on the field will be fired, so
the entire text field will be selected.

") (text . "") (text . "  The method for retrieving the current selected text differs from one
  browser to another. A number of jQuery plug-ins offer cross-platform
  solutions.
") (text . "")) ("examples" ((text . "") (text . "To do something when text in input boxes is selected:

") (text . "") (js . "
	      $(\":input\").select( function () { 
	      $(\"div\").text(\"Something was selected\").show().fadeOut(1000); 
	      });
	      ") (text . "") (css . "
	      p { color:blue; }
	      div { color:red; }
	      ") (text . "") (html . "<p>
	      
	      Click and drag the mouse to select text in the inputs.
	      </p>
	      <input type=\"text\" value=\"Some text\" />
	      <input type=\"text\" value=\"to test on\" />
	      
	      <div></div>") (text . "")) ((text . "") (text . "To trigger the select event on all input elements, try:

") (text . "") (js . "$(\"input\").select();") (text . ""))))) jquery-doc-hash)

(push "change" jquery-methods)

(puthash "change" (quote (("name" . "change") ("signatures" "change" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"change\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼchangeʼ, handler) in the first two
variations, and .trigger(ʼchangeʼ) in the third.

") (text . "") (text . "The change event is sent to an element when its value changes. This
event is limited to <input> elements, <textarea> boxes and <select>
elements. For select boxes, checkboxes, and radio buttons, the event is
fired immediately when the user makes a selection with the mouse, but
for the other element types the event is deferred until the element
loses focus.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<form>
	      <input class=\"target\" type=\"text\" value=\"Field 1\" />
	      <select class=\"target\">
	      <option value=\"option1\" selected=\"selected\">Option 1</option>
	      <option value=\"option2\">Option 2</option>
	      </select>
	      </form>
	      <div id=\"other\">
	      Trigger the handler
	      </div>") (text . "") (text . "The event handler can be bound to the text input and the select box:


") (text . "") (js . "$('.target').change(function() {
	      alert('Handler for .change() called.');
	      });") (text . "") (text . "Now when the second option is selected from the dropdown, the alert is
displayed. It is also displayed if you change the text in the field and
then click away. If the field loses focus without the contents having
changed, though, the event is not triggered. To trigger the event
manually, apply .change() without arguments:
") (text . "") (js . "$('#other').click(function() {
	      $('.target').change();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message. The message will display twice, because the handler has
been bound to the change event on both of the form elements.
") (text . "") (text . "As of jQuery 1.4, the change event bubbles in Internet Explorer,
behaving consistently with the event in other modern browsers.

") (text . "")) ("examples" ((text . "") (text . "Attaches a change event to the select that gets the text for each
selected option and writes them in the div. It then triggers the event
for the initial text draw.
") (text . "") (js . "
	      $(\"select\").change(function () {
	      var str = \"\";
	      $(\"select option:selected\").each(function () {
	      str += $(this).text() + \" \";
	      });
	      $(\"div\").text(str);
	      })
	      .change();
	      ") (text . "") (css . "
	      
	      div { color:red; }
	      ") (text . "") (html . "<select name=\"sweets\" multiple=\"multiple\">
	      <option>Chocolate</option>
	      <option selected=\"selected\">Candy</option>
	      
	      <option>Taffy</option>
	      <option selected=\"selected\">Caramel</option>
	      <option>Fudge</option>
	      <option>Cookie</option>
	      
	      </select>
	      <div></div>") (text . "")) ((text . "") (text . "To add a validity test to all text input elements:

") (text . "") (js . "$(\"input[type='text']\").change( function() {
	      // check input ($(this).val()) for validity here
	      });") (text . ""))))) jquery-doc-hash)

(push "blur" jquery-methods)

(puthash "blur" (quote (("name" . "blur") ("signatures" "blur" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"blur\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼblurʼ, handler) in the first two
variations, and .trigger(ʼblurʼ) in the third.

") (text . "") (text . "The blur event is sent to an element when it loses focus. Originally,
this event was only applicable to form elements, such as <input>. In
recent browsers, the domain of the event has been extended to include
all element types. An element can lose focus via keyboard commands,
such as the Tab key, or by mouse clicks elsewhere on the page.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<form>
	      <input id=\"target\" type=\"text\" value=\"Field 1\" />
	      <input type=\"text\" value=\"Field 2\" />
	      </form>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      The event handler can be bound to the first input field:
	      $('#target').blur(function() {
	      alert('Handler for .blur() called.');
	      });") (text . "") (text . "Now if the first field has the focus, clicking elsewhere or tabbing
away from it displays the alert:

") (text . "") (text . "Handler for .blur() called.


") (text . "") (text . "To trigger the event programmatically, apply .blur() without an
argument:

") (text . "") (js . "$('#other').click(function() {
	      $('#target').blur();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message.

") (text . "") (text . "The blur event does not bubble in Internet Explorer. Therefore, scripts
that rely on event delegation with the blur event will not work
consistently across browsers. As of version 1.4.2, however, jQuery
works around this limitation by mapping blur to the focusout event in
its event delegation methods, .live() and .delegate() .
") (text . "")) ("examples" ((text . "") (text . "To trigger the blur event on all paragraphs:

") (text . "") (js . "$(\"p\").blur();") (text . ""))))) jquery-doc-hash)

(push "focus" jquery-methods)

(puthash "focus" (quote (("name" . "focus") ("signatures" "focus" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"focus\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "  * This method is a shortcut for .bind(ʼfocusʼ, handler) in the first
    and second variations, and .trigger(ʼfocusʼ) in the third.
  * The focus event is sent to an element when it gains focus. This
    event is implicitly applicable to a limited set of elements, such
    as form elements ( <input>, <select>, etc.) and links ( <a href>).
    In recent browser versions, the event can be extended to include
    all element types by explicitly setting the elementʼs tabindex
    property. An element can gain focus via keyboard commands, such as
    the Tab key, or by mouse clicks on the element.
  * Elements with focus are usually highlighted in some way by the
    browser, for example with a dotted line surrounding the element.
    The focus is used to determine which element is the first to
    receive keyboard-related events.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<form>
	      <input id=\"target\" type=\"text\" value=\"Field 1\" />
	      <input type=\"text\" value=\"Field 2\" />
	      </form>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      ") (text . "") (text . "The event handler can be bound to the first input field:


") (text . "") (js . "$('#target').focus(function() {
	      alert('Handler for .focus() called.');
	      });") (text . "") (text . "Now clicking on the first field, or tabbing to it from another field,
displays the alert:

") (text . "") (text . "Handler for .focus() called.


") (text . "") (text . "We can trigger the event when another element is clicked:


") (text . "") (js . "$('#other').click(function() {
	      $('#target').focus();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message.

") (text . "") (text . "The focus event does not bubble in Internet Explorer. Therefore,
scripts that rely on event delegation with the focus event will not
work consistently across browsers. As of version 1.4.2, however, jQuery
works around this limitation by mapping focus to the focusin event in
its event delegation methods, .live() and .delegate() .
") (text . "") (text . "  Triggering the focus on hidden elements causes an error in Internet
  Explorer. Take care to only call .focus() without parameters on
  elements that are visible.
") (text . "")) ("examples" ((text . "") (text . "Fire focus.

") (text . "") (css . "span {display:none;}") (text . "") (js . "
	      $(\"input\").focus(function () {
	      $(this).next(\"span\").css('display','inline').fadeOut(1000);
	      });
	      ") (text . "") (html . "<p><input type=\"text\" /> <span>focus fire</span></p>

	      <p><input type=\"password\" /> <span>focus fire</span></p>") (text . "")) ((text . "") (text . "To stop people from writing in text input boxes, try:

") (text . "") (js . "$(\"input[type=text]\").focus(function(){
	      $(this).blur();
	      });") (text . "")) ((text . "") (text . "To focus on a login input box with id ʼloginʼ on page startup, try:

") (text . "") (js . "$(document).ready(function(){
	      $(\"#login\").focus();
	      });") (text . ""))))) jquery-doc-hash)

(push "mousemove" jquery-methods)

(puthash "mousemove" (quote (("name" . "mousemove") ("signatures" "mousemove" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"mousemove\" JavaScript event, or trigger
that event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼmousemoveʼ, handler) in the first
two variations, and .trigger(ʼmousemoveʼ) in the third.

") (text . "") (text . "The mousemove event is sent to an element when the mouse pointer moves
inside the element. Any HTML element can receive this event.

") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<div id=\"target\">
	      Move here
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      <div id=\"log\"></div>") (text . "") (text . "The event handler can be bound to the target:


") (text . "") (js . "$(\"#target\").mousemove(function(event) {
	      var msg = \"Handler for .mousemove() called at \";
	      msg += event.pageX + \", \" + event.pageY;
	      $(\"#log\").append(\"<div>\" + msg + \"</div>\");
	      });") (text . "") (text . "Now when the mouse pointer moves within the target button, the messages
are appended to <div id=\"log\">:

") (text . "") (text . "Handler for .mousemove() called at (399, 48)
Handler for .mousemove() called at (398, 46)
Handler for .mousemove() called at (397, 44)
Handler for .mousemove() called at (396, 42)
") (text . "") (text . "To trigger the event manually, apply .mousemove() without an argument:


") (text . "") (js . "$(\"#other\").click(function() {
	      $(\"#target\").mousemove();
	      });") (text . "") (text . "After this code executes, clicks on the Trigger button will also append
the message:

") (text . "") (text . "Handler for .mousemove() called at (undefined, undefined)


") (text . "") (text . "When tracking mouse movement, you usually need to know the actual
position of the mouse pointer. The event object that is passed to the
handler contains some information about the mouse coordinates.
Properties such as .clientX, .offsetX, and .pageX are available, but
support for them differs between browsers. Fortunately, jQuery
normalizes the .pageX and .pageY properties so that they can be used in
all browsers. These properties provide the X and Y coordinates of the
mouse pointer relative to the top-left corner of the document, as
illustrated in the example output above.
") (text . "") (text . "Keep in mind that the mousemove event is triggered whenever the mouse
pointer moves, even for a pixel. This means that hundreds of events can
be generated over a very small amount of time. If the handler has to do
any significant processing, or if multiple handlers for the event
exist, this can be a serious performance drain on the browser. It is
important, therefore, to optimize mousemove handlers as much as
possible, and to unbind them as soon as they are no longer needed.
") (text . "") (text . "A common pattern is to bind the mousemove handler from within a
mousedown hander, and to unbind it from a corresponding mouseup
handler. If implementing this sequence of events, remember that the
mouseup event might be sent to a different HTML element than the
mousemove event was. To account for this, the mouseup handler should
typically be bound to an element high up in the DOM tree, such as
<body>.
") (text . "")) ("examples" ((text . "") (text . "Show the mouse coordinates when the mouse is moved over the yellow div.
Coordinates are relative to the window, which in this case is the
iframe.
") (text . "") (js . "
	      $(\"div\").mousemove(function(e){
	      var pageCoords = \"( \" + e.pageX + \", \" + e.pageY + \" )\";
	      var clientCoords = \"( \" + e.clientX + \", \" + e.clientY + \" )\";
	      $(\"span:first\").text(\"( e.pageX, e.pageY ) - \" + pageCoords);
	      $(\"span:last\").text(\"( e.clientX, e.clientY ) - \" + clientCoords);
	      });
	      
	      ") (text . "") (text . "300

") (text . "") (css . "
	      div { width:220px; height:170px; margin;10px; margin-right:50px;
	      background:yellow; border:2px groove; float:right; }
	      p { margin:0; margin-left:10px; color:red; width:220px;
	      height:120px; padding-top:70px;
	      float:left; font-size:14px; }
	      span { display:block; }
	      ") (text . "") (html . "<p>   
	      Try scrolling too.
	      <span>Move the mouse over the div.</span>
	      <span>&nbsp;</span>
	      </p>
	      
	      <div></div>") (text . ""))))) jquery-doc-hash)

(push "hover" jquery-methods)

(puthash "hover" (quote (("name" . "hover") ("signatures" "hover" (("handlerIn(eventObject)" "A function to execute when the mouse pointer enters the element.

" nil nil) ("handlerOut(eventObject)" "A function to execute when the mouse pointer leaves the element.

" nil nil))) ("desc" (text . "Bind two handlers to the matched elements, to be executed when the
mouse pointer enters and leaves the elements.

")) ("longdesc" (text . "") (text . "The .hover() method binds handlers for both mouseenter and mouseleave
events. You can use it to simply apply behavior to an element during
the time the mouse is within the element.
") (text . "") (text . "Calling $(selector).hover(handlerIn, handlerOut) is shorthand for:


") (text . "") (js . "$(selector).mouseenter(handlerIn).mouseleave(handlerOut);") (text . "") (text . "See the discussions for .mouseenter() and .mouseleave() for more
details.

") (text . "")) ("examples" ((text . "") (text . "To add a special style to list items that are being hovered over, try:

") (text . "") (js . "
	      $(\"li\").hover(
	      function () {
	      $(this).append($(\"<span> ***</span>\"));
	      }, 
	      function () {
	      $(this).find(\"span:last\").remove();
	      }
	      );
	      
	      
	      
	      //li with fade class
	      $(\"li.fade\").hover(function(){$(this).fadeOut(100);$(this).fadeIn(500);});
	      
	      ") (text . "") (css . "
	      ul { margin-left:20px; color:blue; }
	      li { cursor:default; }
	      span { color:red; }
	      ") (text . "") (html . "<ul>
	      <li>Milk</li>
	      <li>Bread</li>
	      <li class='fade'>Chips</li>
	      
	      <li class='fade'>Socks</li>
	      </ul>") (text . "")) ((text . "") (text . "To add a special style to table cells that are being hovered over, try:


") (text . "") (js . "$(\"td\").hover(
	      function () {
	      $(this).addClass(\"hover\");
	      },
	      function () {
	      $(this).removeClass(\"hover\");
	      }
	      );") (text . "")) ((text . "") (text . "To unbind the above example use:

") (text . "") (js . "$(\"td\").unbind('mouseenter mouseleave');") (text . ""))))) jquery-doc-hash)

(push "hover" jquery-methods)

(puthash "hover" (quote (("name" . "hover") ("signatures" "hover" (("handlerInOut(eventObject)" "A function to execute when the mouse pointer enters or leaves the
element.

" nil nil))) ("desc" (text . "Bind a single handler to the matched elements, to be executed when the
mouse pointer enters or leaves the elements.

")) ("longdesc" (text . "") (text . "The .hover() method, when passed a single function, will execute that
handler for both mouseenter and mouseleave events. This allows the user
to use jQueryʼs various toggle methods within the handler or to respond
differently within the handler depending on the event.type.
") (text . "") (text . "Calling $(selector).hover(handlerInOut) is shorthand for:


") (text . "") (js . "$(selector).bind(\"mouseenter mouseleave\", handlerInOut);") (text . "") (text . "See the discussions for .mouseenter() and .mouseleave() for more
details.

") (text . "")) ("examples" ((text . "") (text . "Slide the next sibling LI up or down on hover, and toggle a class.

") (text . "") (js . "
	      $(\"li\")
	      .filter(\":odd\")
	      .hide()
	      .end()
	      .filter(\":even\")
	      .hover(
	      function () {
	      $(this).toggleClass(\"active\")
	      .next().stop(true, true).slideToggle();
	      }
	      );
	      
	      
	      ") (text . "") (css . "
	      ul { margin-left:20px; color:blue; }
	      li { cursor:default; }
	      li.active { background:black;color:white; }
	      span { color:red; }
	      ") (text . "") (html . "<ul>
	      <li>Milk</li>
	      <li>White</li>
	      <li>Carrots</li>
	      <li>Orange</li>
	      <li>Broccoli</li>
	      <li>Green</li>
	      </ul>") (text . ""))))) jquery-doc-hash)

(push "mouseleave" jquery-methods)

(puthash "mouseleave" (quote (("name" . "mouseleave") ("signatures" "mouseleave" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to be fired when the mouse leaves an element, or
trigger that handler on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼmouseleaveʼ, handler) in the first
two variations, and .trigger(ʼmouseleaveʼ) in the third.

") (text . "") (text . "The mouseleave JavaScript event is proprietary to Internet Explorer.
Because of the eventʼs general utility, jQuery simulates this event so
that it can be used regardless of browser. This event is sent to an
element when the mouse pointer leaves the element. Any HTML element can
receive this event.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<div id=\"outer\">
	      Outer
	      <div id=\"inner\">
	      Inner
	      </div>
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      <div id=\"log\"></div>") (text . "") (text . "

") (text . "") (text . "The event handler can be bound to any element:


") (text . "") (js . "$('#outer').mouseleave(function() {
	      $('#log').append('<div>Handler for .mouseleave() called.</div>');
	      });") (text . "") (text . "Now when the mouse pointer moves out of the Outer <div>, the message is
appended to <div id=\"log\">. You can also trigger the event when another
element is clicked:
") (text . "") (js . "$('#other').click(function() {
	      $('#outer').mouseleave();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also
append the message.

") (text . "") (text . "The mouseleave event differs from mouseout in the way it handles event
bubbling. If mouseout were used in this example, then when the mouse
pointer moved out of the Inner element, the handler would be triggered.
This is usually undesirable behavior. The mouseleave event, on the
other hand, only triggers its handler when the mouse leaves the element
it is bound to, not a descendant. So in this example, the handler is
triggered when the mouse leaves the Outer element, but not the Inner
element.
") (text . "")) ("examples" ((text . "") (text . "Show number of times mouseout and mouseleave events are triggered.
mouseout fires when the pointer moves out of child element as well,
while mouseleave fires only when the pointer moves out of the bound
element.
") (text . "") (css . "
	      div.out {
	      width:40%;
	      height:120px;
	      margin:0 15px;
	      background-color:#D6EDFC;
	      float:left;
	      }
	      div.in {
	      width:60%;
	      height:60%;
	      background-color:#FFCC00;
	      margin:10px auto;
	      }
	      p {
	      line-height:1em;
	      margin:0;
	      padding:0;
	      }
	      ") (text . "") (js . "
	      var i = 0;
	      $(\"div.overout\").mouseover(function(){
	      $(\"p:first\",this).text(\"mouse over\");
	      }).mouseout(function(){
	      $(\"p:first\",this).text(\"mouse out\");
	      $(\"p:last\",this).text(++i);
	      });

	      var n = 0;
	      $(\"div.enterleave\").mouseenter(function(){
	      $(\"p:first\",this).text(\"mouse enter\");
	      }).mouseleave(function(){
	      $(\"p:first\",this).text(\"mouse leave\");
	      $(\"p:last\",this).text(++n);
	      });

	      ") (text . "") (html . "
	      <div class=\"out overout\"><p>move your mouse</p><div class=\"in overout\"><p>move your mouse</p><p>0</p></div><p>0</p></div>

	      <div class=\"out enterleave\"><p>move your mouse</p><div class=\"in enterleave\"><p>move your mouse</p><p>0</p></div><p>0</p></div>

	      ") (text . ""))))) jquery-doc-hash)

(push "mouseenter" jquery-methods)

(puthash "mouseenter" (quote (("name" . "mouseenter") ("signatures" "mouseenter" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to be fired when the mouse enters an element, or
trigger that handler on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼmouseenterʼ, handler) in the first
two variations, and .trigger(ʼmouseenterʼ) in the third.

") (text . "") (text . "The mouseenter JavaScript event is proprietary to Internet Explorer.
Because of the eventʼs general utility, jQuery simulates this event so
that it can be used regardless of browser. This event is sent to an
element when the mouse pointer enters the element. Any HTML element can
receive this event.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<div id=\"outer\">
	      Outer
	      <div id=\"inner\">
	      Inner
	      </div>
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      <div id=\"log\"></div>") (text . "") (text . "

") (text . "") (text . "The event handler can be bound to any element:


") (text . "") (js . "$('#outer').mouseenter(function() {
	      $('#log').append('<div>Handler for .mouseenter() called.</div>');
	      });") (text . "") (text . "Now when the mouse pointer moves over the Outer <div>, the message is
appended to <div id=\"log\">. You can also trigger the event when another
element is clicked:
") (text . "") (js . "$('#other').click(function() {
	      $('#outer').mouseenter();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also
append the message.

") (text . "") (text . "The mouseenter event differs from mouseover in the way it handles event
bubbling. If mouseover were used in this example, then when the mouse
pointer moved over the Inner element, the handler would be triggered.
This is usually undesirable behavior. The mouseenter event, on the
other hand, only triggers its handler when the mouse enters the element
it is bound to, not a descendant. So in this example, the handler is
triggered when the mouse enters the Outer element, but not the Inner
element.
") (text . "")) ("examples" ((text . "") (text . "Show texts when mouseenter and mouseout event triggering. mouseover
fires when the pointer moves into the child element as well, while
mouseenter fires only when the pointer moves into the bound element.
") (text . "") (css . "
	      div.out {
	      width:40%;
	      height:120px;
	      margin:0 15px;
	      background-color:#D6EDFC;
	      float:left;
	      }
	      div.in {
	      width:60%;
	      height:60%;
	      background-color:#FFCC00;
	      margin:10px auto;
	      }
	      p {
	      line-height:1em;
	      margin:0;
	      padding:0;
	      }
	      ") (text . "") (js . "
	      var i = 0;
	      $(\"div.overout\").mouseover(function(){
	      $(\"p:first\",this).text(\"mouse over\");
	      $(\"p:last\",this).text(++i);
	      }).mouseout(function(){
	      $(\"p:first\",this).text(\"mouse out\");
	      });

	      var n = 0;
	      $(\"div.enterleave\").mouseenter(function(){
	      $(\"p:first\",this).text(\"mouse enter\");
	      $(\"p:last\",this).text(++n);
	      }).mouseleave(function(){
	      $(\"p:first\",this).text(\"mouse leave\");
	      });

	      ") (text . "") (html . "
	      <div class=\"out overout\"><p>move your mouse</p><div class=\"in overout\"><p>move your mouse</p><p>0</p></div><p>0</p></div>

	      <div class=\"out enterleave\"><p>move your mouse</p><div class=\"in enterleave\"><p>move your mouse</p><p>0</p></div><p>0</p></div>

	      ") (text . ""))))) jquery-doc-hash)

(push "mouseout" jquery-methods)

(puthash "mouseout" (quote (("name" . "mouseout") ("signatures" "mouseout" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"mouseout\" JavaScript event, or trigger
that event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼmouseoutʼ, handler) in the first
two variation, and .trigger(ʼmouseoutʼ) in the third.

") (text . "") (text . "The mouseout event is sent to an element when the mouse pointer leaves
the element. Any HTML element can receive this event.

") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<div id=\"outer\">
	      Outer
	      <div id=\"inner\">
	      Inner
	      </div>
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      <div id=\"log\"></div>") (text . "") (text . "

") (text . "") (text . "The event handler can be bound to any element:


") (text . "") (js . "$('#outer').mouseout(function() {
	      $('#log').append('Handler for .mouseout() called.');
	      });") (text . "") (text . "Now when the mouse pointer moves out of the Outer <div>, the message is
appended to <div id=\"log\">. To trigger the event manually, apply
.mouseout() without an argument::
") (text . "") (js . "$('#other').click(function() {
	      $('#outer').mouseout();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also
append the message.

") (text . "") (text . "This event type can cause many headaches due to event bubbling. For
instance, when the mouse pointer moves out of the Inner element in this
example, a mouseout event will be sent to that, then trickle up to
Outer. This can trigger the bound mouseout handler at inopportune
times. See the discussion for . mouseleave() for a useful alternative.
") (text . "")) ("examples" ((text . "") (text . "Show the number of times mouseout and mouseleave events are triggered.
mouseout fires when the pointer moves out of the child element as well,
while mouseleave fires only when the pointer moves out of the bound
element.
") (text . "") (css . "
	      div.out {
	      width:40%;
	      height:120px;
	      margin:0 15px;
	      background-color:#D6EDFC;
	      float:left;
	      }
	      div.in {
	      width:60%;
	      height:60%;
	      background-color:#FFCC00;
	      margin:10px auto;
	      }
	      p {
	      line-height:1em;
	      margin:0;
	      padding:0;
	      }
	      ") (text . "") (js . "
	      var i = 0;
	      $(\"div.overout\").mouseout(function(){
	      $(\"p:first\",this).text(\"mouse out\");
	      $(\"p:last\",this).text(++i);
	      }).mouseover(function(){
	      $(\"p:first\",this).text(\"mouse over\");
	      });

	      var n = 0;
	      $(\"div.enterleave\").bind(\"mouseenter\",function(){
	      $(\"p:first\",this).text(\"mouse enter\");
	      }).bind(\"mouseleave\",function(){
	      $(\"p:first\",this).text(\"mouse leave\");
	      $(\"p:last\",this).text(++n);
	      });

	      ") (text . "") (html . "
	      <div class=\"out overout\"><p>move your mouse</p><div class=\"in overout\"><p>move your mouse</p><p>0</p></div><p>0</p></div>

	      <div class=\"out enterleave\"><p>move your mouse</p><div class=\"in enterleave\"><p>move your mouse</p><p>0</p></div><p>0</p></div>

	      ") (text . ""))))) jquery-doc-hash)

(push "mouseover" jquery-methods)

(puthash "mouseover" (quote (("name" . "mouseover") ("signatures" "mouseover" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"mouseover\" JavaScript event, or trigger
that event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼmouseoverʼ, handler) in the first
two variations, and .trigger(ʼmouseoverʼ) in the third.

") (text . "") (text . "The mouseover event is sent to an element when the mouse pointer enters
the element. Any HTML element can receive this event.

") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<div id=\"outer\">
	      Outer
	      <div id=\"inner\">
	      Inner
	      </div>
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      <div id=\"log\"></div>") (text . "") (text . "

") (text . "") (text . "The event handler can be bound to any element:


") (text . "") (js . "$('#outer').mouseover(function() {
	      $('#log').append('<div>Handler for .mouseover() called.</div>');
	      });") (text . "") (text . "Now when the mouse pointer moves over the Outer <div>, the message is
appended to <div id=\"log\">. We can also trigger the event when another
element is clicked:
") (text . "") (js . "$('#other').click(function() {
	      $('#outer').mouseover();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also
append the message.

") (text . "") (text . "This event type can cause many headaches due to event bubbling. For
instance, when the mouse pointer moves over the Inner element in this
example, a mouseover event will be sent to that, then trickle up to
Outer. This can trigger our bound mouseover handler at inopportune
times. See the discussion for .mouseenter() for a useful alternative.
") (text . "")) ("examples" ((text . "") (text . "Show the number of times mouseover and mouseenter events are triggered.
mouseover fires when the pointer moves into the child element as well,
while mouseenter fires only when the pointer moves into the bound
element.
") (text . "") (css . "
	      div.out { width:40%; height:120px; margin:0 15px;
	      background-color:#D6EDFC; float:left; }
	      div.in {  width:60%; height:60%; 
	      background-color:#FFCC00; margin:10px auto; }
	      p { line-height:1em; margin:0; padding:0; }
	      ") (text . "") (js . "
	      var i = 0;
	      $(\"div.overout\").mouseover(function() {
	      i += 1;
	      $(this).find(\"span\").text( \"mouse over x \" + i );
	      }).mouseout(function(){
	      $(this).find(\"span\").text(\"mouse out \");
	      });

	      var n = 0;
	      $(\"div.enterleave\").mouseenter(function() {
	      n += 1;
	      $(this).find(\"span\").text( \"mouse enter x \" + n );
	      }).mouseleave(function() {
	      $(this).find(\"span\").text(\"mouse leave\");
	      });

	      ") (text . "") (html . "
	      <div class=\"out overout\">
	      <span>move your mouse</span>
	      <div class=\"in\">
	      </div>
	      </div>

	      <div class=\"out enterleave\">
	      <span>move your mouse</span>
	      <div class=\"in\">
	      </div>
	      </div>
	      ") (text . ""))))) jquery-doc-hash)

(push "dblclick" jquery-methods)

(puthash "dblclick" (quote (("name" . "dblclick") ("signatures" "dblclick" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"dblclick\" JavaScript event, or trigger
that event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼdblclickʼ, handler) in the first
two variations, and .trigger(ʼdblclickʼ) in the third. The dblclick
event is sent to an element when the element is double-clicked. Any
HTML element can receive this event. For example, consider the HTML:
") (text . "") (js . "<div id=\"target\">
	      Double-click here
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>") (text . "") (text . "

") (text . "") (text . "The event handler can be bound to any <div>:


") (text . "") (js . "$('#target').dblclick(function() {
	      alert('Handler for .dblclick() called.');
	      });") (text . "") (text . "Now double-clicking on this element displays the alert:


") (text . "") (text . "Handler for .dblclick() called.


") (text . "") (text . "To trigger the event manually, apply .dblclick() without an argument:


") (text . "") (js . "$('#other').click(function() {
	      $('#target').dblclick();
	      });") (text . "") (text . "After this code executes, (single) clicks on Trigger the handler will
also alert the message.

") (text . "") (text . "The dblclick event is only triggered after this exact series of events:


") (text . "") (text . "  * The mouse button is depressed while the pointer is inside the
    element.
  * The mouse button is released while the pointer is inside the
    element.
  * The mouse button is depressed again while the pointer is inside the
    element, within a time window that is system-dependent.
  * The mouse button is released while the pointer is inside the
    element.
") (text . "") (text . "It is inadvisable to bind handlers to both the click and dblclick
events for the same element. The sequence of events triggered varies
from browser to browser, with some receiving two click events before
the dblclick and others only one. Double-click sensitivity (maximum
time between clicks that is detected as a double click) can vary by
operating system and browser, and is often user-configurable.
") (text . "")) ("examples" ((text . "") (text . "To bind a \"Hello World!\" alert box the dblclick event on every
paragraph on the page:

") (text . "") (js . "$(\"p\").dblclick( function () { alert(\"Hello World!\"); });") (text . "")) ((text . "") (text . "Double click to toggle background color.

") (text . "") (js . "
	      var divdbl = $(\"div:first\");
	      divdbl.dblclick(function () { 
	      divdbl.toggleClass('dbl'); 
	      });

	      ") (text . "") (css . "

	      div { background:blue;
	      color:white;
	      height:100px;
	      width:150px;
	      }
	      div.dbl { background:yellow;color:black; }
	      ") (text . "") (html . "<div></div><span>Double click the block</span>") (text . ""))))) jquery-doc-hash)

(push "click" jquery-methods)

(puthash "click" (quote (("name" . "click") ("signatures" "click" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"click\" JavaScript event, or trigger that
event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼclickʼ, handler) in the first two
variations, and .trigger(ʼclickʼ) in the third.

") (text . "") (text . "The click event is sent to an element when the mouse pointer is over
the element, and the mouse button is pressed and released. Any HTML
element can receive this event.
") (text . "") (js . "For example, consider the HTML:
	      <div id=\"target\">
	      Click here
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>") (text . "") (text . "

") (text . "") (text . "The event handler can be bound to any <div>:


") (text . "") (js . "$('#target').click(function() {
	      alert('Handler for .click() called.');
	      });") (text . "") (text . "Now if we click on this element, the alert is displayed:


") (text . "") (text . "Handler for .click() called.


") (text . "") (text . "We can also trigger the event when a different element is clicked:


") (text . "") (js . "$('#other').click(function() {
	      $('#target').click();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message.

") (text . "") (text . "The click event is only triggered after this exact series of events:


") (text . "") (text . "  * The mouse button is depressed while the pointer is inside the
    element.
  * The mouse button is released while the pointer is inside the
    element.
") (text . "") (text . "This is usually the desired sequence before taking an action. If this
is not required, the mousedown or mouseup event may be more suitable.

") (text . "")) ("examples" ((text . "") (text . "To hide paragraphs on a page when they are clicked:

") (text . "") (js . "
	      $(\"p\").click(function () { 
	      $(this).slideUp(); 
	      });
	      $(\"p\").hover(function () {
	      $(this).addClass(\"hilite\");
	      }, function () {
	      $(this).removeClass(\"hilite\");
	      });
	      ") (text . "") (css . "
	      p { color:red; margin:5px; cursor:pointer; }
	      p.hilite { background:yellow; }
	      ") (text . "") (html . "<p>First Paragraph</p>
	      
	      <p>Second Paragraph</p>
	      <p>Yet one more Paragraph</p>") (text . "")) ((text . "") (text . "To trigger the click event on all of the paragraphs on the page:

") (text . "") (js . "$(\"p\").click();") (text . ""))))) jquery-doc-hash)

(push "mouseup" jquery-methods)

(puthash "mouseup" (quote (("name" . "mouseup") ("signatures" "mouseup" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"mouseup\" JavaScript event, or trigger
that event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼmouseupʼ, handler) in the first
variation, and .trigger(ʼmouseupʼ) in the second.

") (text . "") (text . "The mouseup event is sent to an element when the mouse pointer is over
the element, and the mouse button is released. Any HTML element can
receive this event.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<div id=\"target\">
	      Click here
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>
	      ") (text . "") (text . "

") (text . "") (text . "The event handler can be bound to any <div>:


") (text . "") (js . "$('#target').mouseup(function() {
	      alert('Handler for .mouseup() called.');
	      });
	      ") (text . "") (text . "Now if we click on this element, the alert is displayed:


") (text . "") (text . "Handler for .mouseup() called.


") (text . "") (text . "We can also trigger the event when a different element is clicked:


") (text . "") (js . "$('#other').click(function() {
	      $('#target').mouseup();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message.

") (text . "") (text . "If the user clicks outside an element, drags onto it, and releases the
button, this is still counted as a mouseup event. This sequence of
actions is not treated as a button press in most user interfaces, so it
is usually better to use the click event unless we know that the
mouseup event is preferable for a particular situation.
") (text . "")) ("examples" ((text . "") (text . "Show texts when mouseup and mousedown event triggering.

") (text . "") (js . "
	      $(\"p\").mouseup(function(){
	      $(this).append('<span style=\"color:#F00;\">Mouse up.</span>');
	      }).mousedown(function(){
	      $(this).append('<span style=\"color:#00F;\">Mouse down.</span>');
	      });

	      ") (text . "") (html . "<p>Press mouse and release here.</p>
	      ") (text . ""))))) jquery-doc-hash)

(push "mousedown" jquery-methods)

(puthash "mousedown" (quote (("name" . "mousedown") ("signatures" "mousedown" (("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) nil) ("desc" (text . "Bind an event handler to the \"mousedown\" JavaScript event, or trigger
that event on an element.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼmousedownʼ, handler) in the first
variation, and .trigger(ʼmousedownʼ) in the second.

") (text . "") (text . "The mousedown event is sent to an element when the mouse pointer is
over the element, and the mouse button is pressed. Any HTML element can
receive this event.
") (text . "") (text . "For example, consider the HTML:


") (text . "") (js . "<div id=\"target\">
	      Click here
	      </div>
	      <div id=\"other\">
	      Trigger the handler
	      </div>") (text . "") (text . "

") (text . "") (text . "The event handler can be bound to any <div>:


") (text . "") (js . "$('#target').mousedown(function() {
	      alert('Handler for .mousedown() called.');
	      });") (text . "") (text . "Now if we click on this element, the alert is displayed:


") (text . "") (text . "Handler for .mousedown() called.


") (text . "") (text . "We can also trigger the event when a different element is clicked:


") (text . "") (js . "$('#other').click(function() {
	      $('#target').mousedown();
	      });") (text . "") (text . "After this code executes, clicks on Trigger the handler will also alert
the message.

") (text . "") (text . "The mousedown event is sent when any mouse button is clicked. To act
only on specific buttons, we can use the event objectʼs which property.
Not all browsers support this property (Internet Explorer uses button
instead), but jQuery normalizes the property so that it is safe to use
in any browser. The value of which will be 1 for the left button, 2 for
the middle button, or 3 for the right button.
") (text . "") (text . "This event is primarily useful for ensuring that the primary button was
used to begin a drag operation; if ignored, strange results can occur
when the user attempts to use a context menu. While the middle and
right buttons can be detected with these properties, this is not
reliable. In Opera and Safari, for example, right mouse button clicks
are not detectable by default.
") (text . "") (text . "If the user clicks on an element, drags away from it, and releases the
button, this is still counted as a mousedown event. This sequence of
actions is treated as a \"canceling\" of the button press in most user
interfaces, so it is usually better to use the click event unless we
know that the mousedown event is preferable for a particular situation.
") (text . "")) ("examples" ((text . "") (text . "Show texts when mouseup and mousedown event triggering.

") (text . "") (js . "
	      $(\"p\").mouseup(function(){
	      $(this).append('<span style=\"color:#F00;\">Mouse up.</span>');
	      }).mousedown(function(){
	      $(this).append('<span style=\"color:#00F;\">Mouse down.</span>');
	      });
	      
	      ") (text . "") (html . "<p>Press mouse and release here.</p>
	      ") (text . ""))))) jquery-doc-hash)

(push "error" jquery-methods)

(puthash "error" (quote (("name" . "error") ("signatures" "error" (("handler(eventObject)" "A function to execute when the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil))) ("desc" (text . "Bind an event handler to the \"error\" JavaScript event.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼerrorʼ, handler).


") (text . "") (text . "The error event is sent to elements, such as images, that are
referenced by a document and loaded by the browser. It is called if the
element was not loaded correctly.
") (text . "") (text . "For example, consider a page with a simple image element:


") (text . "") (js . "<img alt=\"Book\" id=\"book\" />") (text . "") (text . "The event handler can be bound to the image:


") (text . "") (js . "$('#book')
	      .error(function() {
	      alert('Handler for .error() called.')
	      })
	      .attr(\"src\", \"missing.png\");
	      ") (text . "") (text . "If the image cannot be loaded (for example, because it is not present
at the supplied URL), the alert is displayed:

") (text . "") (text . "Handler for .error() called.


") (text . "") (text . "  The event handler must be attached before the browser fires the
  error event, which is why the example sets the src attribute after
  attaching the handler. Also, the error event may not be correctly
  fired when the page is served locally; error relies on HTTP status
  codes and will generally not be triggered if the URL uses the file:
  protocol.
") (text . "") (text . "Note: A jQuery error event handler should not be attached to the window
object. The browser fires the windowʼs error event when a script error
occurs. However, the window error event receives different arguments
and has different return value requirements than conventional event
handlers. Use window.onerror instead.
") (text . "")) ("examples" ((text . "") (text . "To hide the \"broken image\" icons for IE users, you can try:

") (text . "") (js . "$(\"img\")
	      .error(function(){
	      $(this).hide();
	      })
	      .attr(\"src\", \"missing.png\");") (text . ""))))) jquery-doc-hash)

(push "unload" jquery-methods)

(puthash "unload" (quote (("name" . "unload") ("signatures" "unload" (("handler(eventObject)" "A function to execute when the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil))) ("desc" (text . "Bind an event handler to the \"unload\" JavaScript event.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼunloadʼ, handler).


") (text . "") (text . "The unload event is sent to the window element when the user navigates
away from the page. This could mean one of many things. The user could
have clicked on a link to leave the page, or typed in a new URL in the
address bar. The forward and back buttons will trigger the event.
Closing the browser window will cause the event to be triggered. Even a
page reload will first create an unload event.
") (text . "") (text . "  The exact handling of the unload event has varied from version to
  version of browsers. For example, some versions of Firefox trigger
  the event when a link is followed, but not when the window is
  closed. In practical usage, behavior should be tested on all
  supported browsers, and contrasted with the proprietary beforeunload
  event.
") (text . "") (text . "Any unload event handler should be bound to the window object:


") (text . "") (js . "$(window).unload(function() {
	      alert('Handler for .unload() called.');
	      });
	      ") (text . "") (text . "After this code executes, the alert will be displayed whenever the
browser leaves the current page. It is not possible to cancel the
unload event with .preventDefault(). This event is available so that
scripts can perform cleanup when the user leaves the page.
") (text . "")) ("examples" ((text . "") (text . "To display an alert when a page is unloaded:

") (text . "") (js . "$(window).unload( function () { alert(\"Bye now!\"); } );") (text . ""))))) jquery-doc-hash)

(push "load" jquery-methods)

(puthash "load" (quote (("name" . "load") ("signatures" "load" (("handler(eventObject)" "A function to execute when the event is triggered.

" nil nil)) (("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil))) ("desc" (text . "Bind an event handler to the \"load\" JavaScript event.

")) ("longdesc" (text . "") (text . "This method is a shortcut for .bind(ʼloadʼ, handler).


") (text . "") (text . "The load event is sent to an element when it and all sub-elements have
been completely loaded. This event can be sent to any element
associated with a URL: images, scripts, frames, iframes, and the window
object.
") (text . "") (text . "For example, consider a page with a simple image:


") (text . "") (js . "<img src=\"book.png\" alt=\"Book\" id=\"book\" />") (text . "") (text . "The event handler can be bound to the image:


") (text . "") (js . "$('#book').load(function() {
	      // Handler for .load() called.
	      });") (text . "") (text . "As soon as the image has been loaded, the handler is called.


") (text . "") (text . "In general, it is not necessary to wait for all images to be fully
loaded. If code can be executed earlier, it is usually best to place it
in a handler sent to the .ready() method.
") (text . "") (text . "  The Ajax module also has a method named .load() . Which one is fired
  depends on the set of arguments passed.

") (text . "") (text . "  Caveats of the load event when used with images

  A common challenge developers attempt to solve using the .load()
  shortcut is to execute a function when an image (or collection of
  images) have completely loaded. There are several known caveats with
  this that should be noted. These are:
  * It doesnʼt work consistently nor reliably cross-browser
  * It doesnʼt fire correctly in WebKit if the image src is set to the
    same src as before
  * It doesnʼt correctly bubble up the DOM tree
  * Can cease to fire for images that already live in the browserʼs
    cache
") (text . "")) ("examples" ((text . "") (text . "Run a function when the page is fully loaded including graphics.

") (text . "") (js . "$(window).load(function () {
	      // run code
	      });") (text . "")) ((text . "") (text . "Add the class bigImg to all images with height greater then 100 upon
each image load.

") (text . "") (js . "$('img.userIcon').load(function(){
	      if($(this).height() > 100) {
	      $(this).addClass('bigImg');
	      }
	      });") (text . ""))))) jquery-doc-hash)

(push "ready" jquery-methods)

(puthash "ready" (quote (("name" . "ready") ("signatures" "ready" (("handler" "A function to execute after the DOM is ready.

" nil nil))) ("desc" (text . "Specify a function to execute when the DOM is fully loaded.

")) ("longdesc" (text . "") (text . "While JavaScript provides the load event for executing code when a page
is rendered, this event does not get triggered until all assets such as
images have been completely received. In most cases, the script can be
run as soon as the DOM hierarchy has been fully constructed. The
handler passed to .ready() is guaranteed to be executed after the DOM
is ready, so this is usually the best place to attach all other event
handlers and run other jQuery code. When using scripts that rely on the
value of CSS style properties, itʼs important to reference external
stylesheets or embed style elements before referencing the scripts.
") (text . "") (text . "In cases where code relies on loaded assets (for example, if the
dimensions of an image are required), the code should be placed in a
handler for the load event instead.
") (text . "") (text . "  The .ready() method is generally incompatible with the <body
  onload=\"\"> attribute. If load must be used, either do not use
  .ready() or use jQueryʼs .load() method to attach load event
  handlers to the window or to more specific items, like images.
") (text . "") (text . "All three of the following syntaxes are equivalent:


") (text . "") (text . "  * $(document).ready(handler)
  * $().ready(handler) (this is not recommended)
  * $(handler)
") (text . "") (text . "There is also $(document).bind(\"ready\", handler). This behaves
similarly to the ready method but with one exception: If the ready
event has already fired and you try to .bind(\"ready\") the bound handler
will not be executed. Ready handlers bound this way are executed after
any bound by the other three methods above.
") (text . "") (text . "The .ready() method can only be called on a jQuery object matching the
current document, so the selector can be omitted.

") (text . "") (text . "The .ready() method is typically used with an anonymous function:


") (text . "") (js . "$(document).ready(function() {
	      // Handler for .ready() called.
	      });") (text . "") (text . "Which is equivalent to calling:


") (text . "") (js . "$(function() {
	      // Handler for .ready() called.
	      });") (text . "") (text . "If .ready() is called after the DOM has been initialized, the new
handler passed in will be executed immediately.

") (text . "") (text . " Aliasing the jQuery Namespace


") (text . "") (text . "When using another JavaScript library, we may wish to call
$.noConflict() to avoid namespace difficulties. When this function is
called, the $ shortcut is no longer available, forcing us to write
jQuery each time we would normally write $. However, the handler passed
to the .ready() method can take an argument, which is passed the global
jQuery object. This means we can rename the object within the context
of our .ready() handler without affecting other code:
") (text . "") (js . "jQuery(document).ready(function($) {
	      // Code using $ as usual goes here.
	      });") (text . "")) ("examples" ((text . "") (text . "Display a message when the DOM is loaded.

") (text . "") (js . "$(document).ready(function () {
	      $(\"p\").text(\"The DOM is now loaded and can be manipulated.\");
	      });") (text . "") (css . "p { color:red; }") (text . "") (html . "<p>Not loaded yet.</p>") (text . ""))))) jquery-doc-hash)

(push "die" jquery-methods)

(puthash "die" (quote (("name" . "die") ("signatures" "die" nil) ("desc" (text . "Remove all event handlers previously attached using .live() from the
elements.

")) ("longdesc" (text . "") (text . "Any handler that has been attached with .live() can be removed with
.die(). This method is analogous to calling .unbind() with no
arguments, which is used to remove all handlers attached with .bind().
See the discussions of .live() and .unbind() for further details.
") (text . "") (text . "Note: Up to jQuery 1.4.4, in order for .die() to function correctly,
the selector used with it must match exactly the selector initially
used with .live().
") (text . "")) ("examples"))) jquery-doc-hash)

(push "die" jquery-methods)

(puthash "die" (quote (("name" . "die") ("signatures" "die" (("eventType" "A string containing a JavaScript event type, such as click or keydown.

" nil nil) ("handler" "The function that is no longer to be executed.

" "true" nil)) (("eventTypes" "A map of one or more event types, such as click or keydown and their
corresponding functions that are no longer to be executed.

" nil nil))) ("desc" (text . "Remove an event handler previously attached using .live() from the
elements.

")) ("longdesc" (text . "") (text . "Any handler that has been attached with .live() can be removed with
.die(). This method is analogous to .unbind(), which is used to remove
handlers attached with .bind(). See the discussions of .live() and
.unbind() for further details.
") (text . "") (text . "Note: Up to jQuery 1.4.4, in order for .die() to function correctly,
the selector used with it must match exactly the selector initially
used with .live().
") (text . "")) ("examples" ((text . "") (text . "Can bind and unbind events to the colored button.

") (text . "") (js . "
	      
	      function aClick() {
	      $(\"div\").show().fadeOut(\"slow\");
	      }
	      $(\"#bind\").click(function () {
	      $(\"#theone\").live(\"click\", aClick)
	      .text(\"Can Click!\");
	      });
	      $(\"#unbind\").click(function () {
	      $(\"#theone\").die(\"click\", aClick)
	      .text(\"Does nothing...\");
	      });
	      
	      ") (text . "") (css . "
	      button { margin:5px; }
	      button#theone { color:red; background:yellow; }
	      ") (text . "") (html . "<button id=\"theone\">Does nothing...</button>
	      <button id=\"bind\">Bind Click</button>
	      <button id=\"unbind\">Unbind Click</button>
	      
	      <div style=\"display:none;\">Click!</div>") (text . "")) ((text . "") (text . "To unbind all live events from all paragraphs, write:

") (text . "") (js . "$(\"p\").die()") (text . "")) ((text . "") (text . "To unbind all live click events from all paragraphs, write:

") (text . "") (js . "$(\"p\").die( \"click\" )") (text . "")) ((text . "") (text . "To unbind just one previously bound handler, pass the function in as
the second argument:

") (text . "") (js . "var foo = function () {
	      // code to handle some kind of event
	      };
	      
	      $(\"p\").live(\"click\", foo); // ... now foo will be called when paragraphs are clicked ...
	      
	      $(\"p\").die(\"click\", foo); // ... foo will no longer be called.") (text . ""))))) jquery-doc-hash)

(push "live" jquery-methods)

(puthash "live" (quote (("name" . "live") ("signatures" "live" (("eventType" "A string containing a JavaScript event type, such as \"click\" or
\"keydown.\" As of jQuery 1.4 the string can contain multiple,
space-separated event types or custom event names, as well.
" nil nil) ("handler" "A function to execute at the time the event is triggered.

" nil nil)) (("eventType" "A string containing a JavaScript event type, such as \"click\" or
\"keydown.\" As of jQuery 1.4 the string can contain multiple,
space-separated event types or custom event names, as well.
" nil nil) ("eventData" "A map of data that will be passed to the event handler.

" nil nil) ("handler" "A function to execute at the time the event is triggered.

" nil nil)) (("events" "A map of one or more JavaScript event types and functions to execute
for them.

" nil nil))) ("desc" (text . "Attach a handler to the event for all elements which match the current
selector, now and in the future.

")) ("longdesc" (text . "") (text . "This method is a variation on the basic .bind() method for attaching
event handlers to elements. When .bind() is called, the elements that
the jQuery object refers to get the handler attached; elements that get
introduced later do not, so they would require another .bind() call.
For instance, consider the HTML:
") (text . "") (js . "<body>
	      <div class=\"clickme\">
	      Click here
	      </div>
	      </body>
	      ") (text . "") (text . "To bind a simple click handler to this element:


") (text . "") (js . "$('.clickme').bind('click', function() {
	      // Bound handler called.
	      });
	      ") (text . "") (text . "When the element is clicked, the handler is called. However, suppose
that after this, another element is added:

") (text . "") (js . "$('body').append('<div class=\"clickme\">Another target</div>');") (text . "") (text . "This new element also matches the selector .clickme, but since it was
added after the call to .bind(), clicks on it will do nothing.

") (text . "") (text . "The .live() method provides an alternative to this behavior. To bind a
click handler to the target element using this method:

") (text . "") (js . "$('.clickme').live('click', function() {
	      // Live handler called.
	      });") (text . "") (text . "And then later add a new element:


") (text . "") (js . "$('body').append('<div class=\"clickme\">Another target</div>');") (text . "") (text . "Then clicks on the new element will also trigger the handler.


") (text . "") (text . "To unbind the click handlers from all <div class=\"clickme\"> that were
bound using .live(), use the .die() method:

") (text . "") (js . "$(\".clickme\").die(\"click\");") (text . "") (text . " Event Delegation


") (text . "") (text . "The .live() method is able to affect elements that have not yet been
added to the DOM through the use of event delegation: a handler bound
to an ancestor element is responsible for events that are triggered on
its descendants. The handler passed to .live() is never bound to an
element; instead, .live() binds a special handler to the root of the
DOM tree. In the example above, when the new element is clicked, the
following steps occur:
") (text . "") (text . " 1. A click event is generated and passed to the <div> for handling.
 2. No handler is directly bound to the <div>, so the event bubbles up
    the DOM tree.
 3. The event bubbles up until it reaches the root of the tree, which
    is where .live() binds its special handlers by default.
    * As of jQuery 1.4, event bubbling can optionally stop at a DOM
    element \"context\".
 4. The special click handler bound by .live() executes.
 5. This handler tests the target of the event object to see whether it
    should continue. This test is performed by checking if
    $(event.target).closest(\".clickme\") is able to locate a matching
    element.
 6. If a matching element is found, the original handler is called on
    it.
") (text . "") (text . "Because the test in step 5 is not performed until the event occurs,
elements can be added at any time and still respond to events.

") (text . "") (text . "See the discussion for .bind() for more information on event binding.


") (text . "") (text . " Multiple Events


") (text . "") (text . "As of jQuery 1.4.1 .live() can accept multiple, space-separated events,
similar to the functionality provided in .bind(). For example, you can
\"live bind\" the mouseover and mouseout events at the same time like so:
") (text . "") (js . "$(\".hoverme\").live(\"mouseover mouseout\", function(event) {
	      if ( event.type == \"mouseover\" ) {
	      // do something on mouseover
	      } else {
	      // do something on mouseout
	      }
	      });") (text . "") (text . "As of jQuery 1.4.3, you can bind multiple live event handlers
simultaneously by passing a map of event type/handler pairs:

") (text . "") (js . "$(\"a\").live({
	      click: function() {
	      // do something on click
	      },
	      mouseover: function() {
	      // do something on mouseover
	      }
	      });") (text . "") (text . " Event Data


") (text . "") (text . "As of jQuery 1.4, the optional eventData parameter is available for
passing additional information to the handler. One handy use of this
parameter is to work around issues caused by closures. See the .bind()
methodʼs \" Passing Event Data\" discussion for more information.
") (text . "") (text . " Event Context


") (text . "") (text . "As of jQuery 1.4, live events can be bound to a DOM element \"context\"
rather than to the default document root. To set this context, use the
jQuery() functionʼs second argument , passing in a single DOM element
(as opposed to a jQuery collection or a selector).
") (text . "") (js . "$(\"div.clickme\", $(\"#container\")[0]).live(\"click\", function() {
	      // Live handler called.
	      });") (text . "") (text . "The live handler in this example is called only when <div
class=\"clickme\"> is a descendant of an element with an ID of
\"container.\"
") (text . "") (text . " Caveats


") (text . "") (text . "The .live() technique is useful, but due to its special approach cannot
be simply substituted for .bind() in all cases. Specific differences
include:
") (text . "") (text . "  * DOM traversal methods are not supported for finding elements to
    send to .live(). Rather, the .live() method should always be called
    directly after a selector, as in the example above.
  * To stop further handlers from executing after one bound using
    .live(), the handler must return false. Calling .stopPropagation()
    will not accomplish this.
  * In jQuery 1.3.x only the following JavaScript events (in addition
    to custom events) could be bound with .live(): click, dblclick,
    keydown, keypress, keyup, mousedown, mousemove, mouseout,
    mouseover, and mouseup.
") (text . "") (text . "  * As of jQuery 1.4 the .live() method supports custom events as well
    as all JavaScript events that bubble.
  * As of jQuery 1.4.1 even focus and blur work with live (mapping to
    the more appropriate, bubbling, events focusin and focusout).
  * As of jQuery 1.4.1 the hover event can be specified (mapping to
    mouseenter and mouseleave, which, in turn, are mapped to mouseover
    and mouseout).
") (text . "")) ("examples" ((text . "") (text . "Click a paragraph to add another. Note that .live() binds the click
event to all paragraphs - even new ones.

") (text . "") (js . "
	      $(\"p\").live(\"click\", function(){
	      $(this).after(\"<p>Another paragraph!</p>\");
	      });
	      ") (text . "") (css . "
	      p { background:yellow; font-weight:bold; cursor:pointer;
	      padding:5px; }
	      p.over { background: #ccc; }
	      span { color:red; }
	      ") (text . "") (html . "<p>Click me!</p>

	      <span></span>") (text . "")) ((text . "") (text . "Cancel a default action and prevent it from bubbling up by returning
false.

") (text . "") (js . "$(\"a\").live(\"click\", function() { return false; })") (text . "")) ((text . "") (text . "Cancel only the default action by using the preventDefault method.

") (text . "") (js . "$(\"a\").live(\"click\", function(event){
	      event.preventDefault();
	      });") (text . "")) ((text . "") (text . "Bind custom events with .live().

") (text . "") (js . "
	      $(\"p\").live(\"myCustomEvent\", function(e, myName, myValue) {
	      $(this).text(\"Hi there!\");
	      $(\"span\").stop().css(\"opacity\", 1)
	      .text(\"myName = \" + myName)
	      .fadeIn(30).fadeOut(1000);
	      });
	      $(\"button\").click(function () {
	      $(\"p\").trigger(\"myCustomEvent\");
	      });
	      ") (text . "") (css . "
	      p { color:red; }
	      span { color:blue; }
	      ") (text . "") (html . "
	      <p>Has an attached custom event.</p>
	      <button>Trigger custom event</button>
	      <span style=\"display:none;\"></span>
	      ") (text . "")) ((text . "") (text . "Use a map to bind multiple live event handlers. Note that .live() binds
the click, mouseover, and mouseout events to all paragraphs â even new
ones.
") (text . "") (js . "
	      $(\"p\").live({
	      click: function() {
	      $(this).after(\"<p>Another paragraph!</p>\");
	      },
	      mouseover: function() {
	      $(this).addClass(\"over\");
	      },
	      mouseout: function() {
	      $(this).removeClass(\"over\");
	      }
	      });
	      ") (text . "") (css . "
	      p { background:yellow; font-weight:bold; cursor:pointer; padding:5px; }
	      p.over { background: #ccc; }
	      span { color:red; }
	      ") (text . "") (html . "
	      <p>Click me!</p>
	      <span></span>
	      ") (text . ""))))) jquery-doc-hash)

(push "triggerHandler" jquery-methods)

(puthash "triggerHandler" (quote (("name" . "triggerHandler") ("signatures" "triggerHandler" (("eventType" "A string containing a JavaScript event type, such as click or submit.

" nil nil) ("extraParameters" "An array of additional parameters to pass along to the event handler.

" nil nil))) ("desc" (text . "Execute all handlers attached to an element for an event.

")) ("longdesc" (text . "") (text . "The .triggerHandler() method behaves similarly to .trigger(), with the
following exceptions:

") (text . "") (text . "  * The .triggerHandler() method does not cause the default behavior of
    an event to occur (such as a form submission).
  * While .trigger() will operate on all elements matched by the jQuery
    object, .triggerHandler() only affects the first matched element.
  * Events created with .triggerHandler() do not bubble up the DOM
    hierarchy; if they are not handled by the target element directly,
    they do nothing.
  * Instead of returning the jQuery object (to allow chaining),
    .triggerHandler() returns whatever value was returned by the last
    handler it caused to be executed. If no handlers are triggered, it
    returns undefined
") (text . "") (text . "For more information on this method, see the discussion for .trigger()
.

") (text . "")) ("examples" ((text . "") (text . "If you called .triggerHandler() on a focus event - the browserʼs
default focus action would not be triggered, only the event handlers
bound to the focus event.
") (text . "") (js . "
	      
	      $(\"#old\").click(function(){
	      $(\"input\").trigger(\"focus\");
	      });
	      $(\"#new\").click(function(){
	      $(\"input\").triggerHandler(\"focus\");
	      });
	      $(\"input\").focus(function(){
	      $(\"<span>Focused!</span>\").appendTo(\"body\").fadeOut(1000);
	      });
	      
	      ") (text . "") (html . "<button id=\"old\">.trigger(\"focus\")</button>
	      <button id=\"new\">.triggerHandler(\"focus\")</button><br/><br/>
	      
	      <input type=\"text\" value=\"To Be Focused\"/>") (text . ""))))) jquery-doc-hash)

(push "trigger" jquery-methods)

(puthash "trigger" (quote (("name" . "trigger") ("signatures" "trigger" (("eventType" "A string containing a JavaScript event type, such as click or submit.

" nil nil) ("extraParameters" "An array of additional parameters to pass along to the event handler.

" nil nil)) (("event" "A jQuery.Event object.

" nil nil))) ("desc" (text . "Execute all handlers and behaviors attached to the matched elements for
the given event type.

")) ("longdesc" (text . "") (text . "Any event handlers attached with .bind() or one of its shortcut methods
are triggered when the corresponding event occurs. They can be fired
manually, however, with the .trigger() method. A call to .trigger()
executes the handlers in the same order they would be if the event were
triggered naturally by the user:
") (text . "") (js . "$('#foo').bind('click', function() {
	      alert($(this).text());
	      });
	      $('#foo').trigger('click');") (text . "") (text . "As of jQuery 1.3, .trigger()ed events bubble up the DOM tree; an event
handler can stop the bubbling by returning false from the handler or
calling the .stopPropagation() method on the event object passed into
the event. Although .trigger() simulates an event activation, complete
with a synthesized event object, it does not perfectly replicate a
naturally-occurring event.
") (text . "") (text . "To trigger handlers bound via jQuery without also triggering the native
event, use .triggerHandler() instead.

") (text . "") (text . "When we define a custom event type using the .bind() method, the second
argument to .trigger() can become useful. For example, suppose we have
bound a handler for the custom event to our element instead of the
built-in click event as we did above:
") (text . "") (js . "$('#foo').bind('custom', function(event, param1, param2) {
	      alert(param1 + \"\\n\" + param2);
	      });
	      $('#foo').trigger('custom', ['Custom', 'Event']);
	      ") (text . "") (text . "The event object is always passed as the first parameter to an event
handler, but if additional parameters are specified during a .trigger()
call as they are here, these parameters will be passed along to the
handler as well.
") (text . "") (text . "Note the difference between the extra parameters weʼre passing here and
the eventData parameter to the .bind() method. Both are mechanisms for
passing information to an event handler, but the extraParameters
argument to .trigger() allows information to be determined at the time
the event is triggered, while the eventData argument to .bind()
requires the information to be already computed at the time the handler
is bound.
") (text . "")) ("examples" ((text . "") (text . "Clicks to button #2 also trigger a click for button #1.

") (text . "") (js . "
	      $(\"button:first\").click(function () {
	      update($(\"span:first\"));
	      });
	      $(\"button:last\").click(function () {
	      $(\"button:first\").trigger('click');

	      update($(\"span:last\"));
	      });

	      function update(j) {
	      var n = parseInt(j.text(), 10);
	      j.text(n + 1);
	      }
	      ") (text . "") (css . "

	      button { margin:10px; }
	      div { color:blue; font-weight:bold; }
	      span { color:red; }
	      ") (text . "") (html . "<button>Button #1</button>
	      <button>Button #2</button>
	      <div><span>0</span> button #1 clicks.</div>

	      <div><span>0</span> button #2 clicks.</div>") (text . "")) ((text . "") (text . "To submit the first form without using the submit() function, try:

") (text . "") (js . "$(\"form:first\").trigger(\"submit\")") (text . "")) ((text . "") (text . "To submit the first form without using the submit() function, try:

") (text . "") (js . "var event = jQuery.Event(\"submit\");
	      $(\"form:first\").trigger(event);
	      if ( event.isDefaultPrevented() ) {
	      // Perform an action...
	      }") (text . "")) ((text . "") (text . "To pass arbitrary data to an event:

") (text . "") (js . "$(\"p\").click( function (event, a, b) {
	      // when a normal click fires, a and b are undefined
	      // for a trigger like below a refers to \"foo\" and b refers to \"bar\"

	      } ).trigger(\"click\", [\"foo\", \"bar\"]);") (text . "")) ((text . "") (text . "To pass arbitrary data through an event object:

") (text . "") (js . "var event = jQuery.Event(\"logged\");
	      event.user = \"foo\";
	      event.pass = \"bar\";
	      $(\"body\").trigger(event);") (text . "")) ((text . "") (text . "Alternative way to pass data through an event object:

") (text . "") (js . "$(\"body\").trigger({
	      type:\"logged\",
	      user:\"foo\",
	      pass:\"bar\"

	      });") (text . ""))))) jquery-doc-hash)

(push "ajaxComplete" jquery-methods)

(puthash "ajaxComplete" (quote (("name" . "ajaxComplete") ("signatures" "ajaxComplete" (("handler(event, XMLHttpRequest, ajaxOptions)" "The function to be invoked.

" nil nil))) ("desc" (text . "Register a handler to be called when Ajax requests complete. This is an
Ajax Event.

")) ("longdesc" (text . "Whenever an Ajax request completes, jQuery triggers the ajaxComplete
event. Any and all handlers that have been registered with the
.ajaxComplete() method are executed at this time.
") (text . "") (text . "To observe this method in action, we can set up a basic Ajax load
request:

") (text . "") (js . "<div class=\"trigger\">Trigger</div>
	    <div class=\"result\"></div>
	    <div class=\"log\"></div>
	    ") (text . "") (text . "We can attach our event handler to any element:


") (text . "") (js . "$('.log').ajaxComplete(function() {
	    $(this).text('Triggered ajaxComplete handler.');
	    });
	    ") (text . "") (text . "Now, we can make an Ajax request using any jQuery method:


") (text . "") (js . "$('.trigger').click(function() {
	    $('.result').load('ajax/test.html');
	    });") (text . "") (text . "When the user clicks the button and the Ajax request completes, the log
message is displayed.

") (text . "") (text . "Note: Because .ajaxComplete() is implemented as a method of jQuery
object instances, we can use the this keyword as we do here to refer to
the selected elements within the callback function.
") (text . "") (text . "All ajaxComplete handlers are invoked, regardless of what Ajax request
was completed. If we must differentiate between the requests, we can
use the parameters passed to the handler. Each time an ajaxComplete
handler is executed, it is passed the event object, the XMLHttpRequest
object, and the settings object that was used in the creation of the
request. For example, we can restrict our callback to only handling
events dealing with a particular URL:
") (text . "") (text . "Note: You can get the returned ajax contents by looking at
xhr.responseXML or xhr.responseHTML for xml and html respectively.

") (text . "") (js . "$('.log').ajaxComplete(function(e, xhr, settings) {
	    if (settings.url == 'ajax/test.html') {
	    $(this).text('Triggered ajaxComplete handler. The result is ' +
	    xhr.responseHTML);
	    }
	    });")) ("examples" ((text . "") (text . "Show a message when an Ajax request completes.

") (text . "") (js . "$(\"#msg\").ajaxComplete(function(event,request, settings){
	      $(this).append(\"<li>Request Complete.</li>\");
	      });") (text . ""))))) jquery-doc-hash)

(push "one" jquery-methods)

(puthash "one" (quote (("name" . "one") ("signatures" "one" (("eventType" "A string containing one or more JavaScript event types, such as \"click\"
or \"submit,\" or custom event names.

" nil nil) ("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute at the time the event is triggered.

" nil nil))) ("desc" (text . "Attach a handler to an event for the elements. The handler is executed
at most once per element.

")) ("longdesc" (text . "") (text . "This method is identical to .bind(), except that the handler is unbound
after its first invocation. For example:

") (text . "") (js . "$(\"#foo\").one(\"click\", function() {
	      alert(\"This will be displayed only once.\");
	      });
	      ") (text . "") (text . "After the code is executed, a click on the element with ID foo will
display the alert. Subsequent clicks will do nothing. This code is
equivalent to:
") (text . "") (js . "$(\"#foo\").bind(\"click\", function( event ) {
	      alert(\"This will be displayed only once.\");
	      $(this).unbind( event );
	      });
	      ") (text . "") (text . "In other words, explicitly calling .unbind() from within a
regularly-bound handler has exactly the same effect.

") (text . "") (text . "If the first argument contains more than one space-separated event
types, the event handler is called once for each event type.

") (text . "")) ("examples" ((text . "") (text . "Tie a one-time click to each div.

") (text . "") (js . "
	      var n = 0;
	      $(\"div\").one(\"click\", function() {
	      var index = $(\"div\").index(this);
	      $(this).css({ 
	      borderStyle:\"inset\",
	      cursor:\"auto\"
	      });
	      $(\"p\").text(\"Div at index #\" + index + \" clicked.\" +
	      \"  That's \" + ++n + \" total clicks.\");
	      });

	      ") (text . "") (css . "
	      div { width:60px; height:60px; margin:5px; float:left;
	      background:green; border:10px outset; 
	      cursor:pointer; }
	      p { color:red; margin:0; clear:left; }
	      ") (text . "") (html . "
	      <div></div>
	      <div></div>
	      <div></div>
	      <div></div>
	      <div></div>

	      <p>Click a green square...</p>
	      ") (text . "")) ((text . "") (text . "To display the text of all paragraphs in an alert box the first time
each of them is clicked:

") (text . "") (js . "$(\"p\").one(\"click\", function(){
	      alert( $(this).text() );
	      });") (text . ""))))) jquery-doc-hash)

(push "serializeArray" jquery-methods)

(puthash "serializeArray" (quote (("name" . "serializeArray") ("signatures" "serializeArray" nil) ("desc" (text . "Encode a set of form elements as an array of names and values.

")) ("longdesc" (text . "The .serializeArray() method creates a JavaScript array of objects,
ready to be encoded as a JSON string. It operates on a jQuery object
representing a set of form elements. The form elements can be of
several types:
") (text . "") (js . "<form>
	    <div><input type=\"text\" name=\"a\" value=\"1\" id=\"a\" /></div>
	    <div><input type=\"text\" name=\"b\" value=\"2\" id=\"b\" /></div>
	    <div><input type=\"hidden\" name=\"c\" value=\"3\" id=\"c\" /></div>
	    <div>
	    <textarea name=\"d\" rows=\"8\" cols=\"40\">4</textarea>
	    </div>
	    <div><select name=\"e\">
	    <option value=\"5\" selected=\"selected\">5</option>
	    <option value=\"6\">6</option>
	    <option value=\"7\">7</option>
	    </select></div>
	    <div>
	    <input type=\"checkbox\" name=\"f\" value=\"8\" id=\"f\" />
	    </div>
	    <div>
	    <input type=\"submit\" name=\"g\" value=\"Submit\" id=\"g\" />
	    </div>
	    </form>") (text . "") (text . "The .serializeArray() method uses the standard W3C rules for successful
controls to determine which elements it should include; in particular
the element cannot be disabled and must contain a name attribute. No
submit button value is serialized since the form was not submitted
using a button. Data from file select elements is not serialized.
") (text . "") (text . "This method can act on a jQuery object that has selected individual
form elements, such as <input>, <textarea>, and <select>. However, it
is typically easier to select the <form> tag itself for serialization:
") (text . "") (js . "$('form').submit(function() {
	    console.log($(this).serializeArray());
	    return false;
	    });") (text . "") (text . "This produces the following data structure (provided that the browser
supports console.log):

") (text . "") (js . "[
	    {
	    name: a
	    value: 1
	    },
	    {
	    name: b
	    value: 2
	    },
	    {
	    name: c
	    value: 3
	    },
	    {
	    name: d
	    value: 4
	    },
	    {
	    name: e
	    value: 5
	    }
	    ]")) ("examples" ((text . "") (text . "Get the values from a form, iterate through them, and append them to a
results display.

") (text . "") (js . "
	      
	      function showValues() {
	      var fields = $(\":input\").serializeArray();
	      $(\"#results\").empty();
	      jQuery.each(fields, function(i, field){
	      $(\"#results\").append(field.value + \" \");
	      });
	      }
	      
	      $(\":checkbox, :radio\").click(showValues);
	      $(\"select\").change(showValues);
	      showValues();
	      ") (text . "") (css . "
	      body, select { font-size:14px; }
	      form { margin:5px; }
	      p { color:red; margin:5px; }
	      b { color:blue; }
	      ") (text . "") (html . "<p><b>Results:</b> <span id=\"results\"></span></p>
	      
	      <form>
	      <select name=\"single\">
	      <option>Single</option>
	      <option>Single2</option>
	      
	      </select>
	      <select name=\"multiple\" multiple=\"multiple\">
	      <option selected=\"selected\">Multiple</option>
	      <option>Multiple2</option>
	      
	      <option selected=\"selected\">Multiple3</option>
	      </select><br/>
	      <input type=\"checkbox\" name=\"check\" value=\"check1\" id=\"ch1\"/>
	      
	      <label for=\"ch1\">check1</label>
	      <input type=\"checkbox\" name=\"check\" value=\"check2\" checked=\"checked\" id=\"ch2\"/>
	      
	      <label for=\"ch2\">check2</label>
	      <input type=\"radio\" name=\"radio\" value=\"radio1\" checked=\"checked\" id=\"r1\"/>
	      
	      <label for=\"r1\">radio1</label>
	      <input type=\"radio\" name=\"radio\" value=\"radio2\" id=\"r2\"/>
	      
	      <label for=\"r2\">radio2</label>
	      </form>") (text . ""))))) jquery-doc-hash)

(push "serialize" jquery-methods)

(puthash "serialize" (quote (("name" . "serialize") ("signatures" "serialize" nil) ("desc" (text . "Encode a set of form elements as a string for submission.

")) ("longdesc" (text . "The .serialize() method creates a text string in standard URL-encoded
notation. It operates on a jQuery object representing a set of form
elements. The form elements can be of several types:
") (text . "") (js . "<form>
	    <div><input type=\"text\" name=\"a\" value=\"1\" id=\"a\" /></div>
	    <div><input type=\"text\" name=\"b\" value=\"2\" id=\"b\" /></div>
	    <div><input type=\"hidden\" name=\"c\" value=\"3\" id=\"c\" /></div>
	    <div>
	    <textarea name=\"d\" rows=\"8\" cols=\"40\">4</textarea>
	    </div>
	    <div><select name=\"e\">
	    <option value=\"5\" selected=\"selected\">5</option>
	    <option value=\"6\">6</option>
	    <option value=\"7\">7</option>
	    </select></div>
	    <div>
	    <input type=\"checkbox\" name=\"f\" value=\"8\" id=\"f\" />
	    </div>
	    <div>
	    <input type=\"submit\" name=\"g\" value=\"Submit\" id=\"g\" />
	    </div>
	    </form>") (text . "") (text . "The .serialize() method can act on a jQuery object that has selected
individual form elements, such as <input>, <textarea>, and <select>.
However, it is typically easier to select the <form> tag itself for
serialization:
") (text . "") (js . "$('form').submit(function() {
	    alert($(this).serialize());
	    return false;
	    });") (text . "") (text . "This produces a standard-looking query string:


") (text . "") (js . "a=1&b=2&c=3&d=4&e=5") (text . "") (text . "Warning: selecting both the form and its children will cause duplicates
in the serialized string.

") (text . "") (text . "Note: Only \"successful controls\" are serialized to the string. No
submit button value is serialized since the form was not submitted
using a button. For a form elementʼs value to be included in the
serialized string, the element must have a name attribute. Data from
file select elements is not serialized.
") (text . "")) ("examples" ((text . "") (text . "Serialize a form to a query string, that could be sent to a server in
an Ajax request.

") (text . "") (js . "
	      function showValues() {
	      var str = $(\"form\").serialize();
	      $(\"#results\").text(str);
	      }
	      $(\":checkbox, :radio\").click(showValues);
	      $(\"select\").change(showValues);
	      showValues();
	      ") (text . "") (css . "
	      body, select { font-size:12px; }
	      form { margin:5px; }
	      p { color:red; margin:5px; font-size:14px; }
	      b { color:blue; }
	      ") (text . "") (text . "200

") (text . "") (html . "

	      <form>
	      <select name=\"single\">
	      <option>Single</option>
	      <option>Single2</option>
	      </select>

	      <br />
	      <select name=\"multiple\" multiple=\"multiple\">
	      <option selected=\"selected\">Multiple</option>
	      <option>Multiple2</option>

	      <option selected=\"selected\">Multiple3</option>
	      </select>
	      <br/>
	      <input type=\"checkbox\" name=\"check\" value=\"check1\" id=\"ch1\"/>

	      <label for=\"ch1\">check1</label>

	      <input type=\"checkbox\" name=\"check\" value=\"check2\" checked=\"checked\" id=\"ch2\"/>

	      <label for=\"ch2\">check2</label>
	      <br />
	      <input type=\"radio\" name=\"radio\" value=\"radio1\" checked=\"checked\" id=\"r1\"/>

	      <label for=\"r1\">radio1</label>
	      <input type=\"radio\" name=\"radio\" value=\"radio2\" id=\"r2\"/>

	      <label for=\"r2\">radio2</label>
	      </form>
	      <p><tt id=\"results\"></tt></p>") (text . ""))))) jquery-doc-hash)

(push "$.ajaxSetup" jquery-methods)

(puthash "$.ajaxSetup" (quote (("name" . "$.ajaxSetup") ("signatures" "$.ajaxSetup" (("options" "A set of key/value pairs that configure the default Ajax request. All
options are optional.

" nil nil))) ("desc" (text . "Set default values for future Ajax requests.

")) ("longdesc" (text . "For details on the settings available for $.ajaxSetup(), see $.ajax() .


") (text . "") (text . "All subsequent Ajax calls using any function will use the new settings,
unless overridden by the individual calls, until the next invocation of
$.ajaxSetup().
") (text . "") (text . "For example, the following sets a default for the url parameter before
pinging the server repeatedly:

") (text . "") (js . "$.ajaxSetup({
	    url: 'ping.php'
	    });") (text . "") (text . "Now each time an Ajax request is made, the \"ping.php\" URL will be used
automatically:

") (text . "") (js . "$.ajax({
	    // url not set here; uses ping.php
	    data: {'name': 'Dan'}
	    });") (text . "") (text . "  Note: Global callback functions should be set with their respective
  global Ajax event handler methodsâ .ajaxStart() , .ajaxStop() ,
  .ajaxComplete() , .ajaxError() , .ajaxSuccess() , .ajaxSend()
  ârather than within the options object for $.ajaxSetup().
") (text . "")) ("examples" ((text . "") (text . "Sets the defaults for Ajax requests to the url \"/xmlhttp/\", disables
global handlers and uses POST instead of GET. The following Ajax
requests then sends some data without having to set anything else.
") (text . "") (js . "$.ajaxSetup({
	      url: \"/xmlhttp/\",
	      global: false,
	      type: \"POST\"

	      });
	      $.ajax({ data: myData });") (text . ""))))) jquery-doc-hash)

(push "ajaxSuccess" jquery-methods)

(puthash "ajaxSuccess" (quote (("name" . "ajaxSuccess") ("signatures" "ajaxSuccess" (("handler(event, XMLHttpRequest, ajaxOptions)" "The function to be invoked.

" nil nil))) ("desc" (text . "Attach a function to be executed whenever an Ajax request completes
successfully. This is an Ajax Event.

")) ("longdesc" (text . "") (text . "Whenever an Ajax request completes successfully, jQuery triggers the
ajaxSuccess event. Any and all handlers that have been registered with
the .ajaxSuccess() method are executed at this time.
") (text . "") (text . "To observe this method in action, we can set up a basic Ajax load
request:

") (text . "") (js . "<div class=\"trigger\">Trigger</div>
	      <div class=\"result\"></div>
	      <div class=\"log\"></div>") (text . "") (text . "We can attach our event handler to any element:


") (text . "") (js . "$('.log').ajaxSuccess(function() {
	      $(this).text('Triggered ajaxSuccess handler.');
	      });") (text . "") (text . "Now, we can make an Ajax request using any jQuery method:


") (text . "") (js . "$('.trigger').click(function() {
	      $('.result').load('ajax/test.html');
	      });") (text . "") (text . "When the user clicks the button and the Ajax request completes
successfully, the log message is displayed.

") (text . "") (text . "Note: Because .ajaxSuccess() is implemented as a method of jQuery
object instances, we can use the this keyword as we do here to refer to
the selected elements within the callback function.
") (text . "") (text . "All ajaxSuccess handlers are invoked, regardless of what Ajax request
was completed. If we must differentiate between the requests, we can
use the parameters passed to the handler. Each time an ajaxSuccess
handler is executed, it is passed the event object, the XMLHttpRequest
object, and the settings object that was used in the creation of the
request. For example, we can restrict our callback to only handling
events dealing with a particular URL:
") (text . "") (text . "Note: You can get the returned ajax contents by looking at
xhr.responseXML or xhr.responseHTML for xml and html respectively.

") (text . "") (js . "$('.log').ajaxSuccess(function(e, xhr, settings) {
	      if (settings.url == 'ajax/test.html') {
	      $(this).text('Triggered ajaxSuccess handler. The ajax response was:' 
	      + xhr.responseHTML );
	      }
	      });") (text . "")) ("examples" ((text . "") (text . "Show a message when an Ajax request completes successfully.

") (text . "") (js . "$(\"#msg\").ajaxSuccess(function(evt, request, settings){
	      $(this).append(\"<li>Successful Request!</li>\");
	      });") (text . ""))))) jquery-doc-hash)

(push "ajaxStop" jquery-methods)

(puthash "ajaxStop" (quote (("name" . "ajaxStop") ("signatures" "ajaxStop" (("handler()" "The function to be invoked.

" nil nil))) ("desc" (text . "Register a handler to be called when all Ajax requests have completed.
This is an Ajax Event.

")) ("longdesc" (text . "") (text . "Whenever an Ajax request completes, jQuery checks whether there are any
other outstanding Ajax requests. If none remain, jQuery triggers the
ajaxStop event. Any and all handlers that have been registered with the
.ajaxStop() method are executed at this time. The ajaxStop event is
also triggered if the last outstanding Ajax request is cancelled by
returning false within the beforeSend callback function.
") (text . "") (text . "To observe this method in action, we can set up a basic Ajax load
request:

") (text . "") (js . "<div class=\"trigger\">Trigger</div>
	      <div class=\"result\"></div>
	      <div class=\"log\"></div>") (text . "") (text . "We can attach our event handler to any element:


") (text . "") (js . "$('.log').ajaxStop(function() {
	      $(this).text('Triggered ajaxStop handler.');
	      });") (text . "") (text . "Now, we can make an Ajax request using any jQuery method:


") (text . "") (js . "$('.trigger').click(function() {
	      $('.result').load('ajax/test.html');
	      });") (text . "") (text . "When the user clicks the button and the Ajax request completes, the log
message is displayed.

") (text . "") (text . "Because .ajaxStop() is implemented as a method of jQuery object
instances, we can use the this keyword as we do here to refer to the
selected elements within the callback function.
") (text . "")) ("examples" ((text . "") (text . "Hide a loading message after all the Ajax requests have stopped.

") (text . "") (js . "$(\"#loading\").ajaxStop(function(){
	      $(this).hide();
	      });") (text . ""))))) jquery-doc-hash)

(push "ajaxStart" jquery-methods)

(puthash "ajaxStart" (quote (("name" . "ajaxStart") ("signatures" "ajaxStart" (("handler()" "The function to be invoked.

" nil nil))) ("desc" (text . "Register a handler to be called when the first Ajax request begins.
This is an Ajax Event.

")) ("longdesc" (text . "Whenever an Ajax request is about to be sent, jQuery checks whether
there are any other outstanding Ajax requests. If none are in progress,
jQuery triggers the ajaxStart event. Any and all handlers that have
been registered with the .ajaxStart() method are executed at this time.
") (text . "") (text . "To observe this method in action, we can set up a basic Ajax load
request:

") (text . "") (js . "<div class=\"trigger\">Trigger</div>
	    <div class=\"result\"></div>
	    <div class=\"log\"></div>") (text . "") (text . "We can attach our event handler to any element:


") (text . "") (js . "$('.log').ajaxStart(function() {
	    $(this).text('Triggered ajaxStart handler.');
	    });") (text . "") (text . "Now, we can make an Ajax request using any jQuery method:


") (text . "") (js . "$('.trigger').click(function() {
	    $('.result').load('ajax/test.html');
	    });") (text . "") (text . "When the user clicks the button and the Ajax request is sent, the log
message is displayed.

") (text . "") (text . "Note: Because .ajaxStart() is implemented as a method of jQuery object
instances, we can use the this keyword as we do here to refer to the
selected elements within the callback function.
") (text . "")) ("examples" ((text . "") (text . "Show a loading message whenever an Ajax request starts (and none is
already active).

") (text . "") (js . "$(\"#loading\").ajaxStart(function(){
	      $(this).show();
	      });") (text . ""))))) jquery-doc-hash)

(push "ajaxSend" jquery-methods)

(puthash "ajaxSend" (quote (("name" . "ajaxSend") ("signatures" "ajaxSend" (("handler(event, jqXHR, ajaxOptions)" "The function to be invoked.

" nil nil))) ("desc" (text . "Attach a function to be executed before an Ajax request is sent. This
is an Ajax Event.

")) ("longdesc" (text . "") (text . "Whenever an Ajax request is about to be sent, jQuery triggers the
ajaxSend event. Any and all handlers that have been registered with the
.ajaxSend() method are executed at this time.
") (text . "") (text . "To observe this method in action, we can set up a basic Ajax load
request:

") (text . "") (js . "<div class=\"trigger\">Trigger</div>
	      <div class=\"result\"></div>
	      <div class=\"log\"></div>") (text . "") (text . "We can attach our event handler to any element:


") (text . "") (js . "$('.log').ajaxSend(function() {
	      $(this).text('Triggered ajaxSend handler.');
	      });") (text . "") (text . "Now, we can make an Ajax request using any jQuery method:


") (text . "") (js . "$('.trigger').click(function() {
	      $('.result').load('ajax/test.html');
	      });") (text . "") (text . "When the user clicks the button and the Ajax request is about to begin,
the log message is displayed.

") (text . "") (text . "Note: Because .ajaxSend() is implemented as a method of jQuery
instances, we can use the this keyword as we do here to refer to the
selected elements within the callback function.
") (text . "") (text . "All ajaxSend handlers are invoked, regardless of what Ajax request is
to be sent. If we must differentiate between the requests, we can use
the parameters passed to the handler. Each time an ajaxSend handler is
executed, it is passed the event object, the jqXHR object (in version
1.4, XMLHttpRequestobject), and the settings object that was used in
the creation of the Ajax request. For example, we can restrict our
callback to only handling events dealing with a particular URL:
") (text . "") (js . "$('.log').ajaxSend(function(e, jqxhr, settings) {
	      if (settings.url == 'ajax/test.html') {
	      $(this).text('Triggered ajaxSend handler.');
	      }
	      });") (text . "")) ("examples" ((text . "") (text . "Show a message before an Ajax request is sent.

") (text . "") (js . "$(\"#msg\").ajaxSend(function(evt, request, settings){
	      $(this).append(\"<li>Starting request at \" + settings.url + \"</li>\");
	      });") (text . ""))))) jquery-doc-hash)

(push "ajaxError" jquery-methods)

(puthash "ajaxError" (quote (("name" . "ajaxError") ("signatures" "ajaxError" (("handler(event, jqXHR, ajaxSettings, thrownError)" "The function to be invoked.

" nil nil))) ("desc" (text . "Register a handler to be called when Ajax requests complete with an
error. This is an Ajax Event.

")) ("longdesc" (text . "Whenever an Ajax request completes with an error, jQuery triggers the
ajaxError event. Any and all handlers that have been registered with
the .ajaxError() method are executed at this time.
") (text . "") (text . "To observe this method in action, set up a basic Ajax load request.


") (text . "") (js . "<button class=\"trigger\">Trigger</button>
	    <div class=\"result\"></div>
	    <div class=\"log\"></div>") (text . "") (text . "Attach the event handler to any element:


") (text . "") (js . "$(\"div.log\").ajaxError(function() {
	    $(this).text( \"Triggered ajaxError handler.\" );
	    });") (text . "") (text . "Now, make an Ajax request using any jQuery method:


") (text . "") (js . "$(\"button.trigger\").click(function() {
	    $(\"div.result\").load( \"ajax/missing.html\" );
	    });") (text . "") (text . "When the user clicks the button and the Ajax request fails, because the
requested file is missing, the log message is displayed.

") (text . "") (text . "Note: Because .ajaxError() is implemented as a method of jQuery object
instances, you can use the this keyword within the callback function to
refer to the selected elements.
") (text . "") (text . "All ajaxError handlers are invoked, regardless of what Ajax request was
completed. To differentiate between the requests, you can use the
parameters passed to the handler. Each time an ajaxError handler is
executed, it is passed the event object, the jqXHR object (prior to
jQuery 1.5, the XHR object), and the settings object that was used in
the creation of the request. If the request failed because JavaScript
raised an exception, the exception object is passed to the handler as a
fourth parameter. For example, to restrict the error callback to only
handling events dealing with a particular URL:
") (text . "") (js . "$( \"div.log\" ).ajaxError(function(e, jqxhr, settings, exception) {
	    if ( settings.url == \"ajax/missing.html\" ) {
	    $(this).text( \"Triggered ajaxError handler.\" );
	    }
	    });") (text . "")) ("examples" ((text . "") (text . "Show a message when an Ajax request fails.

") (text . "") (js . "$(\"#msg\").ajaxError(function(event, request, settings){
	      $(this).append(\"<li>Error requesting page \" + settings.url + \"</li>\");
	      });") (text . ""))))) jquery-doc-hash)

(push "unbind" jquery-methods)

(puthash "unbind" (quote (("name" . "unbind") ("signatures" "unbind" (("eventType" "A string containing a JavaScript event type, such as click or submit.

" "true" nil) ("handler(eventObject)" "The function that is to be no longer executed.

" "true" nil)) (("eventType" "A string containing a JavaScript event type, such as click or submit.

" nil nil) ("false" "Unbinds the corresponding ʼreturn falseʼ function that was bound using
.bind( eventType, false ).

" nil nil)) (("event" "A JavaScript event object as passed to an event handler.

" nil nil))) ("desc" (text . "Remove a previously-attached event handler from the elements.

")) ("longdesc" (text . "") (text . "Any handler that has been attached with .bind() can be removed with
.unbind(). In the simplest case, with no arguments, .unbind() removes
all handlers attached to the elements:
") (text . "") (js . "$('#foo').unbind();") (text . "") (text . "This version removes the handlers regardless of type. To be more
precise, we can pass an event type:

") (text . "") (js . "$('#foo').unbind('click');") (text . "") (text . "By specifying the click event type, only handlers for that event type
will be unbound. This approach can still have negative ramifications if
other scripts might be attaching behaviors to the same element,
however. Robust and extensible applications typically demand the
two-argument version for this reason:
") (text . "") (js . "var handler = function() {
	      alert('The quick brown fox jumps over the lazy dog.');
	      };
	      $('#foo').bind('click', handler);
	      $('#foo').unbind('click', handler);
	      ") (text . "") (text . "By naming the handler, we can be assured that no other functions are
caught in the crossfire. Note that the following will not work:

") (text . "") (js . "$('#foo').bind('click', function() {
	      alert('The quick brown fox jumps over the lazy dog.');
	      });

	      // will NOT work
	      $('#foo').unbind('click', function() {
	      alert('The quick brown fox jumps over the lazy dog.');
	      });") (text . "") (text . "Even though the two functions are identical in content, they are
created separately and so JavaScript is free to keep them as distinct
function objects. To unbind a particular handler, we need a reference
to that function and not a different one that happens to do the same
thing.
") (text . "") (text . " Using Namespaces


") (text . "") (text . "Instead of maintaining references to handlers in order to unbind them,
we can namespace the events and use this capability to narrow the scope
of our unbinding actions. As shown in the discussion for the .bind()
method, namespaces are defined by using a period ( .) character when
binding a handler:
") (text . "") (js . "$('#foo').bind('click.myEvents', handler);") (text . "") (text . "When a handler is bound in this fashion, we can still unbind it the
normal way:

") (text . "") (js . "$('#foo').unbind('click');") (text . "") (text . "However, if we want to avoid affecting other handlers, we can be more
specific:

") (text . "") (js . "$('#foo').unbind('click.myEvents');") (text . "") (text . "If multiple namespaced handlers are bound, we can unbind them at once:


") (text . "") (js . "$('#foo').unbind('click.myEvents.yourEvents');") (text . "") (text . "This syntax is similar to that used for CSS class selectors; they are
not hierarchical. This method call is thus the same as:

") (text . "") (js . "$('#foo').unbind('click.yourEvents.myEvents');") (text . "") (text . "We can also unbind all of the handlers in a namespace, regardless of
event type:

") (text . "") (js . "$('#foo').unbind('.myEvents');") (text . "") (text . "It is particularly useful to attach namespaces to event bindings when
we are developing plug-ins or otherwise writing code that may interact
with other event-handling code in the future.
") (text . "") (text . " Using the Event Object


") (text . "") (text . "The third form of the .unbind() method is used when we wish to unbind a
handler from within itself. For example, suppose we wish to trigger an
event handler only three times:
") (text . "") (js . "var timesClicked = 0;
	      $('#foo').bind('click', function(event) {
	      alert('The quick brown fox jumps over the lazy dog.');
	      timesClicked++;
	      if (timesClicked >= 3) {
	      $(this).unbind(event);
	      }
	      });
	      ") (text . "") (text . "The handler in this case must take a parameter, so that we can capture
the event object and use it to unbind the handler after the third
click. The event object contains the context necessary for .unbind() to
know which handler to remove. This example is also an illustration of a
closure. Since the handler refers to the timesClicked variable, which
is defined outside the function, incrementing the variable has an
effect even between invocations of the handler.
") (text . "")) ("examples" ((text . "") (text . "Can bind and unbind events to the colored button.

") (text . "") (js . "

	      function aClick() {
	      $(\"div\").show().fadeOut(\"slow\");
	      }
	      $(\"#bind\").click(function () {
	      // could use .bind('click', aClick) instead but for variety...
	      $(\"#theone\").click(aClick)
	      .text(\"Can Click!\");
	      });
	      $(\"#unbind\").click(function () {
	      $(\"#theone\").unbind('click', aClick)
	      .text(\"Does nothing...\");
	      });

	      ") (text . "") (css . "
	      button { margin:5px; }
	      button#theone { color:red; background:yellow; }
	      ") (text . "") (html . "<button id=\"theone\">Does nothing...</button>
	      <button id=\"bind\">Bind Click</button>
	      <button id=\"unbind\">Unbind Click</button>

	      <div style=\"display:none;\">Click!</div>") (text . "")) ((text . "") (text . "To unbind all events from all paragraphs, write:

") (text . "") (js . "$(\"p\").unbind()") (text . "")) ((text . "") (text . "To unbind all click events from all paragraphs, write:

") (text . "") (js . "$(\"p\").unbind( \"click\" )") (text . "")) ((text . "") (text . "To unbind just one previously bound handler, pass the function in as
the second argument:

") (text . "") (js . "var foo = function () {
	      // code to handle some kind of event
	      };

	      $(\"p\").bind(\"click\", foo); // ... now foo will be called when paragraphs are clicked ...

	      $(\"p\").unbind(\"click\", foo); // ... foo will no longer be called.") (text . ""))))) jquery-doc-hash)

(push "bind" jquery-methods)

(puthash "bind" (quote (("name" . "bind") ("signatures" "bind" (("eventType" "A string containing one or more JavaScript event types, such as \"click\"
or \"submit,\" or custom event names.

" nil nil) ("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("handler(eventObject)" "A function to execute each time the event is triggered.

" nil nil)) (("eventType" "A string containing one or more JavaScript event types, such as \"click\"
or \"submit,\" or custom event names.

" nil nil) ("eventData" "A map of data that will be passed to the event handler.

" "true" nil) ("false" "Setting the third argument to false will attach a function that
prevents the default action from occurring and stops the event from
bubbling.
" nil nil)) (("events" "A map of one or more JavaScript event types and functions to execute
for them.

" nil nil))) ("desc" (text . "Attach a handler to an event for the elements.

")) ("longdesc" (text . "The .bind() method is the primary means of attaching behavior to a
document. All JavaScript event types, such as focus, mouseover, and
resize, are allowed for eventType. (The beforeunload and error events
on the window object use nonstandard conventions and are not supported
by jQuery; attach a handler directly to the window object instead.)
") (text . "") (text . "The jQuery library provides shortcut methods for binding the standard
event types, such as .click() for .bind(ʼclickʼ). A description of each
can be found in the discussion of its shortcut method: blur, focus,
focusin, focusout, load, resize, scroll, unload, click, dblclick,
mousedown, mouseup, mousemove, mouseover, mouseout, mouseenter,
mouseleave, change, select, submit, keydown, keypress, keyup, error
") (text . "") (text . "Any string is legal for eventType; if the string is not the name of a
native JavaScript event, then the handler is bound to a custom event.
These events are never called by the browser, but may be triggered
manually from other JavaScript code using .trigger() or
.triggerHandler().
") (text . "") (text . "If the eventType string contains a period ( .) character, then the
event is namespaced. The period character separates the event from its
namespace. For example, in the call .bind(ʼclick.nameʼ, handler), the
string click is the event type, and the string name is the namespace.
Namespacing allows us to unbind or trigger some events of a type
without affecting others. See the discussion of .unbind() for more
information.
") (text . "") (text . "When an event reaches an element, all handlers bound to that event type
for the element are fired. If there are multiple handlers registered,
they will always execute in the order in which they were bound. After
all handlers have executed, the event continues along the normal event
propagation path.
") (text . "") (text . "A basic usage of .bind() is:


") (text . "") (js . "
	      $('#foo').bind('click', function() {
	      alert('User clicked on \"foo.\"');
	      });
	    ") (text . "") (text . "This code will cause the element with an ID of foo to respond to the
click event. When a user clicks inside this element thereafter, the
alert will be shown.
") (text . "") (text . " Multiple Events


") (text . "") (text . "Multiple event types can be bound at once by including each one
separated by a space:

") (text . "") (js . "
	      $('#foo').bind('mouseenter mouseleave', function() {
	      $(this).toggleClass('entered');
	      });
	    ") (text . "") (text . "The effect of this on <div id=\"foo\"> (when it does not initially have
the \"entered\" class) is to add the \"entered\" class when the mouse
enters the <div> and remove the class when the mouse leaves.
") (text . "") (text . "As of jQuery 1.4 we can bind multiple event handlers simultaneously by
passing a map of event type/handler pairs:

") (text . "") (js . "
	      $('#foo').bind({
	      click: function() {
	      // do something on click
	      },
	      mouseenter: function() {
	      // do something on mouseenter
	      }
	      });
	    ") (text . "") (text . " Event Handlers


") (text . "") (text . "The handler parameter takes a callback function, as shown above. Within
the handler, the keyword this refers to the DOM element to which the
handler is bound. To make use of the element in jQuery, it can be
passed to the normal $() function. For example:
") (text . "") (js . "$('#foo').bind('click', function() {
	    alert($(this).text());
	    });
	    ") (text . "") (text . "After this code is executed, when the user clicks inside the element
with an ID of foo, its text contents will be shown as an alert.

") (text . "") (text . "As of jQuery 1.4.2 duplicate event handlers can be bound to an element
instead of being discarded. For example:

") (text . "") (js . "function test(){ alert(\"Hello\"); }
	    $(\"button\").click( test );
	    $(\"button\").click( test );") (text . "") (text . "The above will generate two alerts when the button is clicked.


") (text . "") (text . "In jQuery 1.4.3 you can now pass in ʼfalseʼ in place of an event
handler. This will bind an event handler thatʼs equivalent to:
function(){ return false; }. This function can be removed at a later
time by calling: .unbind( eventName, false ).
") (text . "") (text . " The Event object


") (text . "") (text . "The handler callback function can also take parameters. When the
function is called, the JavaScript event object will be passed to the
first parameter.
") (text . "") (text . "The event object is often unnecessary and the parameter omitted, as
sufficient context is usually available when the handler is bound to
know exactly what needs to be done when the handler is triggered.
However, at times it becomes necessary to gather more information about
the userʼs environment at the time the event was initiated. View the
full Event Object.
") (text . "") (text . "Returning false from a handler is equivalent to calling both
.preventDefault() and .stopPropagation() on the event object.

") (text . "") (text . "Using the event object in a handler looks like this:


") (text . "") (js . "$(document).ready(function() {
	    $('#foo').bind('click', function(event) {
	    alert('The mouse cursor is at ('
	    + event.pageX + ', ' + event.pageY + ')');
	    });
	    });
	    ") (text . "") (text . "Note the parameter added to the anonymous function. This code will
cause a click on the element with ID foo to report the page coordinates
of the mouse cursor at the time of the click.
") (text . "") (text . " Passing Event Data


") (text . "") (text . "The optional eventData parameter is not commonly used. When provided,
this argument allows us to pass additional information to the handler.
One handy use of this parameter is to work around issues caused by
closures. For example, suppose we have two event handlers that both
refer to the same external variable:
") (text . "") (js . "var message = 'Spoon!';
	    $('#foo').bind('click', function() {
	    alert(message);
	    });
	    message = 'Not in the face!';
	    $('#bar').bind('click', function() {
	    alert(message);
	    });
	    ") (text . "") (text . "Because the handlers are closures that both have message in their
environment, both will display the message Not in the face! when
triggered. The variableʼs value has changed. To sidestep this, we can
pass the message in using eventData:
") (text . "") (js . "var message = 'Spoon!';
	    $('#foo').bind('click', {msg: message}, function(event) {
	    alert(event.data.msg);
	    });
	    message = 'Not in the face!';
	    $('#bar').bind('click', {msg: message}, function(event) {
	    alert(event.data.msg);
	    });
	    ") (text . "") (text . "This time the variable is not referred to directly within the handlers;
instead, the variable is passed in by value through eventData, which
fixes the value at the time the event is bound. The first handler will
now display Spoon! while the second will alert Not in the face!
") (text . "") (text . "  Note that objects are passed to functions by reference, which
  further complicates this scenario.

") (text . "") (text . "If eventData is present, it is the second argument to the .bind()
method; if no additional data needs to be sent to the handler, then the
callback is passed as the second and final argument.
") (text . "") (text . "  See the .trigger() method reference for a way to pass data to a
  handler at the time the event happens rather than when the handler
  is bound.
") (text . "") (text . "As of jQuery 1.4 we can no longer attach data (and thus, events) to
object, embed, or applet elements because critical errors occur when
attaching data to Java applets.
") (text . "") (text . "Note: Although demonstrated in the next example, it is inadvisable to
bind handlers to both the click and dblclick events for the same
element. The sequence of events triggered varies from browser to
browser, with some receiving two click events before the dblclick and
others only one. Double-click sensitivity (maximum time between clicks
that is detected as a double click) can vary by operating system and
browser, and is often user-configurable.
") (text . "")) ("examples" ((text . "") (text . "Handle click and double-click for the paragraph. Note: the coordinates
are window relative, so in this case relative to the demo iframe.

") (text . "") (js . "
	    $(\"p\").bind(\"click\", function(event){
	    var str = \"( \" + event.pageX + \", \" + event.pageY + \" )\";
	    $(\"span\").text(\"Click happened! \" + str);
	    });
	    $(\"p\").bind(\"dblclick\", function(){
	    $(\"span\").text(\"Double-click happened in \" + this.nodeName);
	    });
	    $(\"p\").bind(\"mouseenter mouseleave\", function(event){
	    $(this).toggleClass(\"over\");
	    });

	    ") (text . "") (css . "
	    p { background:yellow; font-weight:bold; cursor:pointer; 
	    padding:5px; }
	    p.over { background: #ccc; }
	    span { color:red; }
	    ") (text . "") (html . "<p>Click or double click here.</p>
	    <span></span>") (text . "")) ((text . "") (text . "To display each paragraphʼs text in an alert box whenever it is
clicked:

") (text . "") (js . "$(\"p\").bind(\"click\", function(){
	    alert( $(this).text() );
	    });") (text . "")) ((text . "") (text . "You can pass some extra data before the event handler:

") (text . "") (js . "function handler(event) {
	    alert(event.data.foo);
	    }
	    $(\"p\").bind(\"click\", {foo: \"bar\"}, handler)") (text . "")) ((text . "") (text . "Cancel a default action and prevent it from bubbling up by returning
false:

") (text . "") (js . "$(\"form\").bind(\"submit\", function() { return false; })") (text . "")) ((text . "") (text . "Cancel only the default action by using the .preventDefault() method.

") (text . "") (js . "$(\"form\").bind(\"submit\", function(event) {
	    event.preventDefault();
	    });") (text . "")) ((text . "") (text . "Stop an event from bubbling without preventing the default action by
using the .stopPropagation() method.

") (text . "") (js . "$(\"form\").bind(\"submit\", function(event) {
	    event.stopPropagation();
	    });") (text . "")) ((text . "") (text . "Bind custom events.

") (text . "") (js . "

	    $(\"p\").bind(\"myCustomEvent\", function(e, myName, myValue){
	    $(this).text(myName + \", hi there!\");
	    $(\"span\").stop().css(\"opacity\", 1)
	    .text(\"myName = \" + myName)
	    .fadeIn(30).fadeOut(1000);
	    });
	    $(\"button\").click(function () {
	    $(\"p\").trigger(\"myCustomEvent\", [ \"John\" ]);
	    });

	    ") (text . "") (css . "
	    p { color:red; }
	    span { color:blue; }
	    ") (text . "") (html . "<p>Has an attached custom event.</p>
	    <button>Trigger custom event</button>
	    <span style=\"display:none;\"></span>") (text . "")) ((text . "") (text . "Bind multiple events simultaneously.

") (text . "") (js . "$(\"div.test\").bind({
	    click: function(){
	    $(this).addClass(\"active\");
	    },
	    mouseenter: function(){
	    $(this).addClass(\"inside\");
	    },
	    mouseleave: function(){
	    $(this).removeClass(\"inside\");
	    }
	    });") (text . ""))))) jquery-doc-hash)

(push "first" jquery-methods)

(puthash "first" (quote (("name" . "first") ("signatures" "first" nil) ("desc" (text . "Reduce the set of matched elements to the first in the set.

")) ("longdesc" (text . "") (text . "Given a jQuery object that represents a set of DOM elements, the
.first() method constructs a new jQuery object from the first matching
element.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
	    <ul>
	    <li>list item 1</li>
	    <li>list item 2</li>
	    <li>list item 3</li>
	    <li>list item 4</li>
	    <li>list item 5</li>
	    </ul>
	  ") (text . "") (text . "We can apply this method to the set of list items:


") (text . "") (js . "$('li').first().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background for the first item.


")) ("examples" ((text . "") (text . "Highlight the first span in a paragraph.

") (text . "") (css . ".highlight{background-color: yellow}") (text . "") (js . "$(\"p span\").first().addClass('highlight');") (text . "") (html . "<p><span>Look:</span> <span>This is some text in a paragraph.</span> <span>This is a note about it.</span></p>") (text . ""))))) jquery-doc-hash)

(push "last" jquery-methods)

(puthash "last" (quote (("name" . "last") ("signatures" "last" nil) ("desc" (text . "Reduce the set of matched elements to the final one in the set.

")) ("longdesc" (text . "") (text . "Given a jQuery object that represents a set of DOM elements, the
.last() method constructs a new jQuery object from the last matching
element.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
	    <ul>
	    <li>list item 1</li>
	    <li>list item 2</li>
	    <li>list item 3</li>
	    <li>list item 4</li>
	    <li>list item 5</li>
	    </ul>
	  ") (text . "") (text . "We can apply this method to the set of list items:


") (text . "") (js . "$('li').last().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background for the final item.


")) ("examples" ((text . "") (text . "Highlight the last span in a paragraph.

") (text . "") (css . ".highlight{background-color: yellow}") (text . "") (js . "$(\"p span\").last().addClass('highlight');") (text . "") (html . "<p><span>Look:</span> <span>This is some text in a paragraph.</span> <span>This is a note about it.</span></p>") (text . ""))))) jquery-doc-hash)

(push "slice" jquery-methods)

(puthash "slice" (quote (("name" . "slice") ("signatures" "slice" (("start" "An integer indicating the 0-based position at which the elements begin
to be selected. If negative, it indicates an offset from the end of the
set.
" nil nil) ("end" "An integer indicating the 0-based position at which the elements stop
being selected. If negative, it indicates an offset from the end of the
set. If omitted, the range continues until the end of the set.
" "true" nil))) ("desc" (text . "Reduce the set of matched elements to a subset specified by a range of
indices.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.slice() method constructs a new jQuery object from a subset of the
matching elements. The supplied start index identifies the position of
one of the elements in the set; if end is omitted, all elements after
this one will be included in the result.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
	    <ul>
	    <li>list item 1</li>
	    <li>list item 2</li>
	    <li>list item 3</li>
	    <li>list item 4</li>
	    <li>list item 5</li>
	    </ul>
	  ") (text . "") (text . "We can apply this method to the set of list items:


") (text . "") (js . "$('li').slice(2).css('background-color', 'red');") (text . "") (text . "The result of this call is a red background for items 3, 4, and 5. Note
that the supplied index is zero-based, and refers to the position of
elements within the jQuery object, not within the DOM tree.
") (text . "") (text . "The end parameter allows us to limit the selected range even further.
For example:

") (text . "") (js . "$('li').slice(2, 4).css('background-color', 'red');") (text . "") (text . "Now only items 3 and 4 are selected. The index is once again
zero-based; the range extends up to but not including the specified
index.
") (text . "") (text . " Negative Indices


") (text . "") (text . "The jQuery .slice() method is patterned after the JavaScript .slice()
method for arrays. One of the features that it mimics is the ability
for negative numbers to be passed as either the start or end parameter.
If a negative number is provided, this indicates a position starting
from the end of the set, rather than the beginning. For example:
") (text . "") (js . "$('li').slice(-2, -1).css('background-color', 'red');") (text . "") (text . "This time only list item 4 is turned red, since it is the only item in
the range between two from the end ( -2) and one from the end ( -1).

")) ("examples" ((text . "") (text . "Turns divs yellow based on a random slice.

") (text . "") (js . "
	    
	    function colorEm() {
	    var $div = $(\"div\");
	    var start = Math.floor(Math.random() *
	    $div.length);
	    var end = Math.floor(Math.random() *
	    ($div.length - start)) +
	    start + 1;
	    if (end == $div.length) end = undefined;
	    $div.css(\"background\", \"\");
	    if (end) 
	    $div.slice(start, end).css(\"background\", \"yellow\");   
	    else
	    $div.slice(start).css(\"background\", \"yellow\");
	    
	    $(\"span\").text('$(\"div\").slice(' + start +
	    (end ? ', ' + end : '') +
	    ').css(\"background\", \"yellow\");');
	    }
	    
	    $(\"button\").click(colorEm);
	    
	    ") (text . "") (css . "
	    div { width:40px; height:40px; margin:10px; float:left;
	    border:2px solid blue; }
	    span { color:red; font-weight:bold; }
	    button { margin:5px; }
	    ") (text . "") (text . "240

") (text . "") (html . "<p><button>Turn slice yellow</button>
	    <span>Click the button!</span></p>
	    <div></div>
	    <div></div>
	    
	    <div></div>
	    <div></div>
	    <div></div>
	    <div></div>
	    <div></div>
	    
	    <div></div>
	    <div></div>") (text . "")) ((text . "") (text . "Selects all paragraphs, then slices the selection to include only the
first element.

") (text . "") (js . "$(\"p\").slice(0, 1).wrapInner(\"<b></b>\");") (text . "")) ((text . "") (text . "Selects all paragraphs, then slices the selection to include only the
first and second element.

") (text . "") (js . "$(\"p\").slice(0, 2).wrapInner(\"<b></b>\");") (text . "")) ((text . "") (text . "Selects all paragraphs, then slices the selection to include only the
second element.

") (text . "") (js . "$(\"p\").slice(1, 2).wrapInner(\"<b></b>\");") (text . "")) ((text . "") (text . "Selects all paragraphs, then slices the selection to include only the
second and third element.

") (text . "") (js . "$(\"p\").slice(1).wrapInner(\"<b></b>\");") (text . "")) ((text . "") (text . "Selects all paragraphs, then slices the selection to include only the
third element.

") (text . "") (js . "$(\"p\").slice(-1).wrapInner(\"<b></b>\");") (text . ""))))) jquery-doc-hash)

(push "jQuery" jquery-methods)

(puthash "jQuery" (quote (("name" . "jQuery") ("signatures" "jQuery" (("selector" "A string containing a selector expression

" nil nil) ("context" "A DOM Element, Document, or jQuery to use as context

" "true" nil)) (("element" "A DOM element to wrap in a jQuery object.

" nil nil)) (("elementArray" "An array containing a set of DOM elements to wrap in a jQuery object.

" nil nil)) (("jQuery object" "An existing jQuery object to clone.

" nil nil)) nil) ("desc" (text . "Accepts a string containing a CSS selector which is then used to match
a set of elements.

")) ("longdesc" (text . "") (text . "In the first formulation listed above, jQuery() â which can also be
written as $() â searches through the DOM for any elements that match
the provided selector and creates a new jQuery object that references
these elements:
") (text . "") (js . "$('div.foo');") (text . "") (text . " Selector Context


") (text . "") (text . "By default, selectors perform their searches within the DOM starting at
the document root. However, an alternate context can be given for the
search by using the optional second parameter to the $() function. For
example, to do a search within an event handler, the search can be
restricted like so:
") (text . "") (js . "
	      $('div.foo').click(function() {
	      $('span', this).addClass('bar');
	      });
	    ") (text . "") (text . "When the search for the span selector is restricted to the context of
this, only spans within the clicked element will get the additional
class.
") (text . "") (text . "Internally, selector context is implemented with the .find() method, so
$(ʼspanʼ, this) is equivalent to $(this).find(ʼspanʼ).

") (text . "") (text . " Using DOM elements


") (text . "") (text . "The second and third formulations of this function create a jQuery
object using one or more DOM elements that were already selected in
some other way. A common use of this facility is to call jQuery methods
on an element that has been passed to a callback function through the
keyword this:
") (text . "") (js . "
	      $('div.foo').click(function() {
	      $(this).slideUp();
	      });
	    ") (text . "") (text . "This example causes elements to be hidden with a sliding animation when
clicked. Because the handler receives the clicked item in the this
keyword as a bare DOM element, the element must be passed to the $()
function before applying jQuery methods to it.
") (text . "") (text . "XML data returned from an Ajax call can be passed to the $() function
so individual elements of the XML structure can be retrieved using
.find() and other DOM traversal methods.
") (text . "") (js . "
	      $.post('url.xml', function(data) {
	      var $child = $(data).find('child');
	      })
	    ") (text . "") (text . " Cloning jQuery Objects


") (text . "") (text . "When a jQuery object is passed to the $() function, a clone of the
object is created. This new jQuery object references the same DOM
elements as the initial one.
") (text . "") (text . " Returning an Empty Set


") (text . "") (text . "As of jQuery 1.4, calling the jQuery() method with no arguments returns
an empty jQuery set (with a .length property of 0). In previous
versions of jQuery, this would return a set containing the document
node.
") (text . "")) ("examples" ((text . "") (text . "Find all p elements that are children of a div element and apply a
border to them.

") (text . "") (js . "
	    $(\"div > p\").css(\"border\", \"1px solid gray\");
	    ") (text . "") (html . "<p>one</p> <div><p>two</p></div> <p>three</p>") (text . "")) ((text . "") (text . "Find all inputs of type radio within the first form in the document.

") (text . "") (js . "$(\"input:radio\", document.forms[0]);") (text . "")) ((text . "") (text . "Find all div elements within an XML document from an Ajax response.

") (text . "") (js . "$(\"div\", xml.responseXML);") (text . "")) ((text . "") (text . "Set the background color of the page to black.

") (text . "") (js . "$(document.body).css( \"background\", \"black\" );") (text . "")) ((text . "") (text . "Hide all the input elements within a form.

") (text . "") (js . "$(myForm.elements).hide()") (text . ""))))) jquery-doc-hash)

(push "jQuery" jquery-methods)

(puthash "jQuery" (quote (("name" . "jQuery") ("signatures" "jQuery" (("html" "A string of HTML to create on the fly. Note that this parses HTML, not
XML.

" nil nil) ("ownerDocument" "A document in which the new elements will be created

" "true" nil)) (("html" "A string defining a single, standalone, HTML element (e.g. <div/> or
<div></div>).

" nil nil) ("props" "An map of attributes, events, and methods to call on the newly-created
element.

" nil nil))) ("desc" (text . "Creates DOM elements on the fly from the provided string of raw HTML.

")) ("longdesc" (text . "") (text . " Creating New Elements


") (text . "") (text . "If a string is passed as the parameter to $(), jQuery examines the
string to see if it looks like HTML (i.e., it has <tag ... > somewhere
within the string). If not, the string is interpreted as a selector
expression, as explained above. But if the string appears to be an HTML
snippet, jQuery attempts to create new DOM elements as described by the
HTML. Then a jQuery object is created and returned that refers to these
elements. You can perform any of the usual jQuery methods on this
object:
") (text . "") (js . "$('<p id=\"test\">My <em>new</em> text</p>').appendTo('body');") (text . "") (text . "If the HTML is more complex than a single tag without attributes, as it
is in the above example, the actual creation of the elements is handled
by the browserʼs innerHTML mechanism. In most cases, jQuery creates a
new <div> element and sets the innerHTML property of the element to the
HTML snippet that was passed in. When the parameter has a single tag,
such as $(ʼ<imgÂ />ʼ) or $(ʼ<a></a>ʼ), jQuery creates the element using
the native JavaScript createElement() function.
") (text . "") (text . "When passing in complex HTML, some browsers may not generate a DOM that
exactly replicates the HTML source provided. As mentioned, we use the
browserʼs .innerHTML property to parse the passed HTML and insert it
into the current document. During this process, some browsers filter
out certain elements such as <html>, <title>, or <head> elements. As a
result, the elements inserted may not be representative of the original
string passed.
") (text . " Filtering isnʼt however just limited to these tags. For example,
Internet Explorer prior to version 8 will also convert all href
properties on links to absolute URLs, and Internet Explorer prior to
version 9 will not correctly handle HTML5 elements without the addition
of a separate compatibility layer.
") (text . "") (text . "To ensure cross-platform compatibility, the snippet must be
well-formed. Tags that can contain other elements should be paired with
a closing tag:
") (text . "") (js . "$('<a href=\"http://jquery.com\"></a>');") (text . "") (text . "Alternatively, jQuery allows XML-like tag syntax (with or without a
space before the slash):

") (text . "") (js . "$('<a/>');") (text . "") (text . "Tags that cannot contain elements may be quick-closed or not:


") (text . "") (js . "$('<img />');
	    $('<input>');
	    ") (text . "") (text . "As of jQuery 1.4, the second argument can accept a map consisting of a
superset of the properties that can be passed to the .attr() method.
Furthermore, any event type can be passed in, and the following jQuery
methods can be called: val, css, html, text, data, width, height, or
offset. Note that Internet Explorer will not allow you to create an
input or button element and change its type; you must specify the type
using ʼ<input type=\"checkbox\" />ʼ for example.
") (text . "")) ("examples" ((text . "") (text . "Create a div element (and all of its contents) dynamically and append
it to the body element. Internally, an element is created and its
innerHTML property set to the given markup.
") (text . "") (js . "$(\"<div><p>Hello</p></div>\").appendTo(\"body\")") (text . "")) ((text . "") (text . "Create some DOM elements.

") (text . "") (js . "$(\"<div/>\", {
	    \"class\": \"test\",
	    text: \"Click me!\",
	    click: function(){
	    $(this).toggleClass(\"test\");
	    }
	    }).appendTo(\"body\");") (text . ""))))) jquery-doc-hash)

(push "jQuery" jquery-methods)

(puthash "jQuery" (quote (("name" . "jQuery") ("signatures" "jQuery" (("callback" "The function to execute when the DOM is ready.

" nil nil))) ("desc" (text . "Binds a function to be executed when the DOM has finished loading.

")) ("longdesc" (text . "") (text . "This function behaves just like $(document).ready(), in that it should
be used to wrap other $() operations on your page that depend on the
DOM being ready. While this function is, technically, chainable, there
really isnʼt much use for chaining against it.
") (text . "")) ("examples" ((text . "") (text . "Execute the function when the DOM is ready to be used.

") (text . "") (js . "$(function(){
	    // Document is ready
	    });
	    ") (text . "")) ((text . "") (text . "Use both the shortcut for $(document).ready() and the argument to write
failsafe jQuery code using the $ alias, without relying on the global
alias.
") (text . "") (js . "jQuery(function($) {
	    // Your code using failsafe $ alias here...
	    });") (text . ""))))) jquery-doc-hash)

(push "stop" jquery-methods)

(puthash "stop" (quote (("name" . "stop") ("signatures" "stop" (("clearQueue" "A Boolean indicating whether to remove queued animation as well.
Defaults to false.

" "true" nil) ("jumpToEnd" "A Boolean indicating whether to complete the current animation
immediately. Defaults to false.

" "true" nil))) ("desc" (text . "Stop the currently-running animation on the matched elements.

")) ("longdesc" (text . "") (text . "When .stop() is called on an element, the currently-running animation
(if any) is immediately stopped. If, for instance, an element is being
hidden with .slideUp() when .stop() is called, the element will now
still be displayed, but will be a fraction of its previous height.
Callback functions are not called.
") (text . "") (text . "If more than one animation method is called on the same element, the
later animations are placed in the effects queue for the element. These
animations will not begin until the first one completes. When .stop()
is called, the next animation in the queue begins immediately. If the
clearQueue parameter is provided with a value of true, then the rest of
the animations in the queue are removed and never run.
") (text . "") (text . "If the jumpToEnd property is provided with a value of true, the current
animation stops, but the element is immediately given its target values
for each CSS property. In our above .slideUp() example, the element
would be immediately hidden. The callback function is then immediately
called, if provided.
") (text . "") (text . "The usefulness of the .stop() method is evident when we need to animate
an element on mouseenter and mouseleave:

") (text . "") (js . "<div id=\"hoverme\">
	    Hover me
	    <img id=\"hoverme\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />
	    </div>") (text . "") (text . "We can create a nice fade effect without the common problem of multiple
queued animations by adding .stop(true, true) to the chain:

") (text . "") (js . "$('#hoverme-stop-2').hover(function() {
	    $(this).find('img').stop(true, true).fadeOut();
	    }, function() {
	    $(this).find('img').stop(true, true).fadeIn();
	    });") (text . "") (text . "  Animations may be stopped globally by setting the property $.fx.off
  to true. When this is done, all animation methods will immediately
  set elements to their final state when called, rather than
  displaying an effect.
") (text . "")) ("examples" ((text . "") (text . "Click the Go button once to start the animation, then click the STOP
button to stop it where itʼs currently positioned. Another option is to
click several buttons to queue them up and see that stop just kills the
currently playing one.
") (text . "") (js . "
	    // Start animation
	    $(\"#go\").click(function(){
	    $(\".block\").animate({left: '+=100px'}, 2000);
	    });
	    
	    // Stop animation when button is clicked
	    $(\"#stop\").click(function(){
	    $(\".block\").stop();
	    });
	    
	    // Start animation in the opposite direction
	    $(\"#back\").click(function(){
	    $(\".block\").animate({left: '-=100px'}, 2000);
	    });
	    
	    ") (text . "") (html . "<button id=\"go\">Go</button> 
	    <button id=\"stop\">STOP!</button>
	    <button id=\"back\">Back</button>
	    <div class=\"block\"></div>") (text . "") (css . "div { 
	    position: absolute; 
	    background-color: #abc;
	    left: 0px;
	    top:30px;
	    width: 60px; 
	    height: 60px;
	    margin: 5px; 
	    }
	    ") (text . ""))))) jquery-doc-hash)

(push "end" jquery-methods)

(puthash "end" (quote (("name" . "end") ("signatures" "end" nil) ("desc" (text . "End the most recent filtering operation in the current chain and return
the set of matched elements to its previous state.

")) ("longdesc" (text . "Most of jQueryʼs DOM traversal methods operate on a jQuery object
instance and produce a new one, matching a different set of DOM
elements. When this happens, it is as if the new set of elements is
pushed onto a stack that is maintained inside the object. Each
successive filtering method pushes a new element set onto the stack. If
we need an older element set, we can use end() to pop the sets back off
of the stack.
") (text . "") (text . "Suppose we have a couple short lists on a page:


") (text . "") (js . "
	    <ul class=\"first\">
	    <li class=\"foo\">list item 1</li>
	    <li>list item 2</li>
	    <li class=\"bar\">list item 3</li>
	    </ul>
	    <ul class=\"second\">
	    <li class=\"foo\">list item 1</li>
	    <li>list item 2</li>
	    <li class=\"bar\">list item 3</li>
	    </ul>
	  ") (text . "") (text . "The end() method is useful primarily when exploiting jQueryʼs chaining
properties. When not using chaining, we can usually just call up a
previous object by variable name, so we donʼt need to manipulate the
stack. With end(), though, we can string all the method calls together:
") (text . "") (js . "
	    $('ul.first').find('.foo').css('background-color', 'red')
	    ") (text . "") (text . "This chain searches for items with the class foo within the first list
only and turns their backgrounds red. Then end() returns the object to
its state before the call to find(), so the second find() looks for
ʼ.barʼ inside <ul class=\"first\">, not just inside that listʼs <li
class=\"foo\">, and turns the matching elementsʼ backgrounds green. The
net result is that items 1 and 3 of the first list have a colored
background, and none of the items from the second list do.
") (text . "") (text . "A long jQuery chain can be visualized as a structured code block, with
filtering methods providing the openings of nested blocks and end()
methods closing them:
") (text . "") (js . "
	    $('ul.first').find('.foo')
	    .css('background-color', 'red')
	    .end().find('.bar')
	    .css('background-color', 'green')
	    .end();
	  ") (text . "") (text . "The last end() is unnecessary, as we are discarding the jQuery object
immediately thereafter. However, when the code is written in this form,
the end() provides visual symmetry and a sense of completion âmaking
the program, at least to the eyes of some developers, more readable, at
the cost of a slight hit to performance as it is an additional function
call.
")) ("examples" ((text . "") (text . "Selects all paragraphs, finds span elements inside these, and reverts
the selection back to the paragraphs.

") (text . "") (js . "
	    
	    jQuery.fn.showTags = function (n) {
	    var tags = this.map(function () { 
	    return this.tagName; 
	    })
	    .get().join(\", \");
	    $(\"b:eq(\" + n + \")\").text(tags);
	    return this;
	    };
	    
	    $(\"p\").showTags(0)
	    .find(\"span\")
	    .showTags(1)
	    .css(\"background\", \"yellow\")
	    .end()
	    .showTags(2)
	    .css(\"font-style\", \"italic\");
	    
	    ") (text . "") (css . "
	    p, div { margin:1px; padding:1px; font-weight:bold; 
	    font-size:16px; }
	    div { color:blue; }
	    b { color:red; }
	    ") (text . "") (html . "<p>
	    Hi there <span>how</span> are you <span>doing</span>?
	    </p>
	    
	    <p>
	    This <span>span</span> is one of 
	    several <span>spans</span> in this
	    <span>sentence</span>.
	    </p>
	    
	    <div>
	    Tags in jQuery object initially: <b></b>
	    </div>
	    <div>
	    Tags in jQuery object after find: <b></b>
	    
	    </div>
	    <div>
	    Tags in jQuery object after end: <b></b>
	    </div>") (text . "")) ((text . "") (text . "Selects all paragraphs, finds span elements inside these, and reverts
the selection back to the paragraphs.

") (text . "") (js . "$(\"p\").find(\"span\").end().css(\"border\", \"2px red solid\");") (text . "") (css . "p { margin:10px; padding:10px; }") (text . "") (html . "<p><span>Hello</span>, how are you?</p>") (text . ""))))) jquery-doc-hash)

(push "andSelf" jquery-methods)

(puthash "andSelf" (quote (("name" . "andSelf") ("signatures" "andSelf" nil) ("desc" (text . "Add the previous set of elements on the stack to the current set.

")) ("longdesc" (text . "As described in the discussion for .end() , jQuery objects maintain an
internal stack that keeps track of changes to the matched set of
elements. When one of the DOM traversal methods is called, the new set
of elements is pushed onto the stack. If the previous set of elements
is desired as well, .andSelf() can help.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
	    <ul>
	    <li>list item 1</li>
	    <li>list item 2</li>
	    <li class=\"third-item\">list item 3</li>
	    <li>list item 4</li>
	    <li>list item 5</li>
	    </ul>
	  ") (text . "") (text . "The result of the following code is a red background behind items 3, 4
and 5:

") (text . "") (js . "$('li.third-item').nextAll().andSelf()
	  .css('background-color', 'red');
	  ") (text . "") (text . "First, the initial selector locates item 3, initializing the stack with
the set containing just this item. The call to .nextAll() then pushes
the set of items 4 and 5 onto the stack. Finally, the .andSelf()
invocation merges these two sets together, creating a jQuery object
that points to all three items in document order:
{[<li.third-item>,<li>,<li> ]}.
")) ("examples" ((text . "") (text . "Find all divs, and all the paragraphs inside of them, and give them
both class names. Notice the div doesnʼt have the yellow background
color since it didnʼt use .andSelf().
") (text . "") (js . "
	    $(\"div\").find(\"p\").andSelf().addClass(\"border\");
	    $(\"div\").find(\"p\").addClass(\"background\");

	    ") (text . "") (css . "
	    p, div { margin:5px; padding:5px; }
	    .border { border: 2px solid red; }
	    .background { background:yellow; }
	    ") (text . "") (html . "<div>
	    <p>First Paragraph</p>
	    <p>Second Paragraph</p>
	    </div>") (text . ""))))) jquery-doc-hash)

(push "siblings" jquery-methods)

(puthash "siblings" (quote (("name" . "siblings") ("signatures" "siblings" (("selector" "A string containing a selector expression to match elements against.

" "true" nil))) ("desc" (text . "Get the siblings of each element in the set of matched elements,
optionally filtered by a selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.siblings() method allows us to search through the siblings of these
elements in the DOM tree and construct a new jQuery object from the
matching elements.
") (text . "") (text . "The method optionally accepts a selector expression of the same type
that we can pass to the $() function. If the selector is supplied, the
elements will be filtered by testing whether they match it.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
	    <ul>
	    <li>list item 1</li>
	    <li>list item 2</li>
	    <li class=\"third-item\">list item 3</li>
	    <li>list item 4</li>
	    <li>list item 5</li>
	    </ul>
	  ") (text . "") (text . "If we begin at the third item, we can find its siblings:


") (text . "") (js . "$('li.third-item').siblings().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind items 1, 2, 4, and
5. Since we do not supply a selector expression, all of the siblings
are part of the object. If we had supplied one, only the matching items
among these four would be included.
") (text . "") (text . "The original element is not included among the siblings, which is
important to remember when we wish to find all elements at a particular
level of the DOM tree.
")) ("examples" ((text . "") (text . "Find the unique siblings of all yellow li elements in the 3 lists
(including other yellow li elements if appropriate).

") (text . "") (js . "
	    
	    var len = $(\".hilite\").siblings()
	    .css(\"color\", \"red\")
	    .length;
	    $(\"b\").text(len);
	    ") (text . "") (css . "
	    ul { float:left; margin:5px; font-size:16px; font-weight:bold; }
	    p { color:blue; margin:10px 20px; font-size:16px; padding:5px; 
	    font-weight:bolder; }
	    .hilite { background:yellow; }
	    ") (text . "") (html . "<ul>
	    <li>One</li>
	    
	    <li>Two</li>
	    <li class=\"hilite\">Three</li>
	    <li>Four</li>
	    </ul>
	    
	    <ul>
	    <li>Five</li>
	    <li>Six</li>
	    <li>Seven</li>
	    
	    </ul>
	    <ul>
	    <li>Eight</li>
	    <li class=\"hilite\">Nine</li>
	    
	    <li>Ten</li>
	    <li class=\"hilite\">Eleven</li>
	    </ul>
	    <p>Unique siblings: <b></b></p>") (text . "")) ((text . "") (text . "Find all siblings with a class \"selected\" of each div.

") (text . "") (js . "$(\"p\").siblings(\".selected\").css(\"background\", \"yellow\");") (text . "") (html . "<div><span>Hello</span></div>
	    
	    <p class=\"selected\">Hello Again</p>
	    <p>And Again</p>") (text . ""))))) jquery-doc-hash)

(push "animate" jquery-methods)

(puthash "animate" (quote (("name" . "animate") ("signatures" "animate" (("properties" "A map of CSS properties that the animation will move toward.

" nil nil) ("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("complete" "A function to call once the animation is complete.

" "true" nil)) (("properties" "A map of CSS properties that the animation will move toward.

" nil nil) ("options" "A map of additional options to pass to the method. Supported keys:
  * duration: A string or number determining how long the animation
    will run.
  * easing: A string indicating which easing function to use for the
    transition.
  * complete: A function to call once the animation is complete.
  * step: A function to be called after each step of the animation.
  * queue: A Boolean indicating whether to place the animation in the
    effects queue. If false, the animation will begin immediately.
  * specialEasing: A map of one or more of the CSS properties defined
    by the properties argument and their corresponding easing functions
    (added 1.4).
" nil nil))) ("desc" (text . "Perform a custom animation of a set of CSS properties.

")) ("longdesc" (text . "") (text . "The .animate() method allows us to create animation effects on any
numeric CSS property. The only required parameter is a map of CSS
properties. This map is similar to the one that can be sent to the
.css() method, except that the range of properties is more restrictive.
") (text . "") (text . " Animation Properties and Values


") (text . "") (text . "All animated properties should be animated to a single numeric value,
except as noted below; most properties that are non-numeric cannot be
animated using basic jQuery functionality. (For example, width, height,
or left can be animated but background-color cannot be.) Property
values are treated as a number of pixels unless otherwise specified.
The units em and % can be specified where applicable.
") (text . "") (text . "In addition to style properties, some non-style properties such as
scrollTop and scrollLeft, as well as custom properties, can be
animated.
") (text . "") (text . "Shorthand CSS properties (e.g. margin, background, border) are not
supported. For example, if you want to retrieve the rendered margin,
use: $(elem).css(ʼmarginTopʼ) and $(elem).css(ʼmarginRightʼ), and so
on.
") (text . "") (text . "In addition to numeric values, each property can take the strings
ʼshowʼ, ʼhideʼ, and ʼtoggleʼ. These shortcuts allow for custom hiding
and showing animations that take into account the display type of the
element.
") (text . "") (text . "Animated properties can also be relative. If a value is supplied with a
leading += or -= sequence of characters, then the target value is
computed by adding or subtracting the given number from the current
value of the property.
") (text . "") (text . "Unlike shorthand animation methods such as .slideDown() and .fadeIn(),
the .animate() method does not make hidden elements visible as part of
the effect. For example, given
$(ʼsomeElementʼ).hide().animate({height:ʼ20pxʼ}, 500}), the animation
will run, but the element will remain hidden.
") (text . "") (text . " Duration


") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively.
") (text . "") (text . " Complete Function


") (text . "") (text . "If supplied, the complete callback function is fired once the animation
is complete. This can be useful for stringing different animations
together in sequence. The callback is not sent any arguments, but this
is set to the DOM element being animated. If multiple elements are
animated, the callback is executed once per matched element, not once
for the animation as a whole.
") (text . "") (text . " Basic Usage


") (text . "") (text . "To animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
	    Click here
	    </div>
	    <img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\"
	    style=\"position: relative; left: 10px;\" />") (text . "") (text . "To animate the opacity, left offset, and height of the image
simultaneously:

") (text . "") (js . "$('#clickme').click(function() {
	    $('#book').animate({
	    opacity: 0.25,
	    left: '+=50',
	    height: 'toggle'
	    }, 5000, function() {
	    // Animation complete.
	    });
	    });
	    ") (text . "") (text . "

") (text . "") (text . "Note that the target value of the height property is ʼtoggleʼ. Since
the image was visible before, the animation shrinks the height to 0 to
hide it. A second click then reverses this transition:
") (text . "") (text . "

") (text . "") (text . "The opacity of the image is already at its target value, so this
property is not animated by the second click. Since the target value
for left is a relative value, the image moves even farther to the right
during this second animation.
") (text . "") (text . "Directional properties ( top, right, bottom, left) have no discernible
effect on elements if their position style property is static, which it
is by default.
") (text . "") (text . "  The jQuery UI project extends the .animate() method by allowing some
  non-numeric styles such as colors to be animated. The project also
  includes mechanisms for specifying animations through CSS classes
  rather than individual attributes.
") (text . "") (text . " Step Function


") (text . "") (text . "The second version of .animate() provides a step option â a callback
function that is fired at each step of the animation. This function is
useful for enabling custom animation types or altering the animation as
it is occurring. It accepts two arguments ( now and fx), and this is
set to the DOM element being animated.
") (text . "") (text . "  * now: the numeric value of the property being animated at each step
  * fx: a reference to the jQuery.fx prototype object, which contains a
    number of properties such as elem for the animated element, start
    and end for the first and last value of the animated property,
    respectively, and prop for the property being animated.
") (text . "") (text . "Note that the step function is called for each animated property on
each animated element. For example, given two list items, the step
function fires four times at each step of the animation:
") (text . "") (js . "$('li').animate({
	    opacity: .5,
	    height: '50%'
	    },
	    {
	    step: function(now, fx) {
	    var data = fx.elem.id + ' ' + fx.prop + ': ' + now;
	    $('body').append('<div>' + data + '</div>');
	    }
	    });") (text . "") (text . " Easing


") (text . "") (text . "The remaining parameter of .animate() is a string naming an easing
function to use. An easing function specifies the speed at which the
animation progresses at different points within the animation. The only
easing implementations in the jQuery library are the default, called
swing, and one that progresses at a constant pace, called linear. More
easing functions are available with the use of plug-ins, most notably
the jQuery UI suite.
") (text . "") (text . " Per-property Easing


") (text . "") (text . "As of jQuery version 1.4, you can set per-property easing functions
within a single .animate() call. In the first version of .animate(),
each property can take an array as its value: The first member of the
array is the CSS property and the second member is an easing function.
If a per-property easing function is not defined for a particular
property, it uses the value of the .animate() methodʼs optional easing
argument. If the easing argument is not defined, the default swing
function is used.
") (text . "") (text . "For example, to simultaneously animate the width and height with the
swing easing function and the opacity with the linear easing function:

") (text . "") (js . "$('#clickme').click(function() {
	    $('#book').animate({
	    width: ['toggle', 'swing'],
	    height: ['toggle', 'swing'],
	    opacity: 'toggle'
	    }, 5000, 'linear', function() {
	    $(this).after('<div>Animation complete.</div>');
	    });
	    });") (text . "") (text . "In the second version of .animate(), the options map can include the
specialEasing property, which is itself a map of CSS properties and
their corresponding easing functions. For example, to simultaneously
animate the width using the linear easing function and the height using
the easeOutBounce easing function:
") (text . "") (js . "$('#clickme').click(function() {
	    $('#book').animate({
	    width: 'toggle',
	    height: 'toggle'
	    }, {
	    duration: 5000,
	    specialEasing: {
	    width: 'linear',
	    height: 'easeOutBounce'
	    },
	    complete: function() {
	    $(this).after('<div>Animation complete.</div>');
	    }
	    });
	    });") (text . "") (text . "As previously noted, a plugin is required for the easeOutBounce
function.

") (text . "")) ("examples" ((text . "") (text . "Click the button to animate the div with a number of different
properties.

") (text . "") (js . "

	    /* Using multiple unit types within one animation. */

	    $(\"#go\").click(function(){
	    $(\"#block\").animate({
	    width: \"70%\",
	    opacity: 0.4,
	    marginLeft: \"0.6in\",
	    fontSize: \"3em\",
	    borderWidth: \"10px\"
	    }, 1500 );
	    });
	    ") (text . "") (html . "<button id=\"go\">&raquo; Run</button>

	    <div id=\"block\">Hello!</div>") (text . "") (css . "
	    div {
	    background-color:#bca;
	    width:100px;
	    border:1px solid green;
	    }
	    ") (text . "")) ((text . "") (text . "Animates a divʼs left property with a relative value. Click several
times on the buttons to see the relative animations queued up.

") (text . "") (js . "
	    $(\"#right\").click(function(){
	    $(\".block\").animate({\"left\": \"+=50px\"}, \"slow\");
	    });

	    $(\"#left\").click(function(){
	    $(\".block\").animate({\"left\": \"-=50px\"}, \"slow\");
	    });

	    ") (text . "") (html . "<button id=\"left\">&laquo;</button> <button id=\"right\">&raquo;</button>
	    <div class=\"block\"></div>
	    ") (text . "") (css . "
	    div {
	    position:absolute;
	    background-color:#abc;
	    left:50px;
	    width:90px;
	    height:90px;
	    margin:5px;
	    }
	    ") (text . "")) ((text . "") (text . "The first button shows how an unqueued animation works. It expands the
div out to 90% width while the font-size is increasing. Once the
font-size change is complete, the border animation will begin. The
second button starts a traditional chained animation, where each
animation will start once the previous animation on the element has
completed.
") (text . "") (js . "

	    $( \"#go1\" ).click(function(){
	    $( \"#block1\" ).animate( { width: \"90%\" }, { queue: false, duration: 3000 })
	    .animate({ fontSize: \"24px\" }, 1500 )
	    .animate({ borderRightWidth: \"15px\" }, 1500 );
	    });

	    $( \"#go2\" ).click(function(){
	    $( \"#block2\" ).animate({ width: \"90%\" }, 1000 )
	    .animate({ fontSize: \"24px\" }, 1000 )
	    .animate({ borderLeftWidth: \"15px\" }, 1000 );
	    });

	    $( \"#go3\" ).click(function(){
	    $( \"#go1\" ).add( \"#go2\" ).click();
	    });

	    $( \"#go4\" ).click(function(){
	    $( \"div\" ).css({ width: \"\", fontSize: \"\", borderWidth: \"\" });
	    });

	    ") (text . "") (html . "<button id=\"go1\">&raquo; Animate Block1</button>
	    <button id=\"go2\">&raquo; Animate Block2</button>
	    <button id=\"go3\">&raquo; Animate Both</button>

	    <button id=\"go4\">&raquo; Reset</button>
	    <div id=\"block1\">Block1</div>
	    <div id=\"block2\">Block2</div>") (text . "") (css . "
	    div {
	    background-color:#bca;
	    width:200px;
	    height:1.1em;
	    text-align:center;
	    border:2px solid green;
	    margin:3px;
	    font-size:14px;
	    }
	    button {
	    font-size:14px;
	    }
	    ") (text . "")) ((text . "") (text . "Animates the first divʼs left property and synchronizes the remaining
divs, using the step function to set their left properties at each
stage of the animation.
") (text . "") (js . "
	    $( \"#go\" ).click(function(){
	    $( \".block:first\" ).animate({
	    left: 100
	    }, {
	    duration: 1000,
	    step: function( now, fx ){
	    $( \".block:gt(0)\" ).css( \"left\", now );
	    }
	    });
	    });
	    ") (text . "") (css . "
	    div {
	    position: relative;
	    background-color: #abc;
	    width: 40px;
	    height: 40px;
	    float: left;
	    margin: 5px;
	    }
	    ") (text . "") (html . "
	    <p><button id=\"go\">Run »</button></p>
	    <div class=\"block\"></div> <div class=\"block\"></div>
	    <div class=\"block\"></div> <div class=\"block\"></div>
	    <div class=\"block\"></div> <div class=\"block\"></div>
	    ") (text . "")) ((text . "") (text . "Animates all paragraphs to toggle both height and opacity, completing
the animation within 600 milliseconds.

") (text . "") (js . "$( \"p\" ).animate({
	    \"height\": \"toggle\", \"opacity\": \"toggle\"
	    }, \"slow\" );") (text . "")) ((text . "") (text . "Animates all paragraph to a left style of 50 and opacity of 1 (opaque,
visible), completing the animation within 500 milliseconds.

") (text . "") (js . "$( \"p\" ).animate({
	    \"left\": \"50\", \"opacity\": 1
	    }, 500 );
	    ") (text . "")) ((text . "") (text . "An example of using an ʼeasingʼ function to provide a different style
of animation. This will only work if you have a plugin that provides
this easing function. Note, this code will do nothing unless the
paragraph element is hidden.
") (text . "") (js . "$( \"p\" ).animate({
	    \"opacity\": \"show\"
	    }, \"slow\", \"easein\" );") (text . "")) ((text . "") (text . "Animates all paragraphs to toggle both height and opacity, completing
the animation within 600 milliseconds.

") (text . "") (js . "$( \"p\" ).animate({
	    \"height\": \"toggle\", \"opacity\": \"toggle\"
	    }, { duration: \"slow\" });") (text . "")) ((text . "") (text . "Animates all paragraph to a left style of 50 and opacity of 1 (opaque,
visible), completing the animation within 500 milliseconds. It also
will do it outside the queue, meaning it will automatically start
without waiting for its turn.
") (text . "") (js . "$( \"p\" ).animate({
	    left: \"50px\", opacity: 1
	    }, { duration: 500, queue: false });") (text . "")) ((text . "") (text . "An example of using an ʼeasingʼ function to provide a different style
of animation. This will only work if you have a plugin that provides
this easing function.
") (text . "") (js . "$( \"p\" ).animate({
	    \"opacity\": \"show\"
	    }, { \"duration\": \"slow\", \"easing\": \"easein\" });") (text . "")) ((text . "") (text . "An example of using a callback function. The first argument is an array
of CSS properties, the second specifies that the animation should take
1000 milliseconds to complete, the third states the easing type, and
the fourth argument is an anonymous callback function.
") (text . "") (js . "$( \"p\" ).animate({
	    height:200, width:400, opacity: .5
	    }, 1000, \"linear\", function(){ alert(\"all done\"); });
	    ") (text . ""))))) jquery-doc-hash)

(push "prevAll" jquery-methods)

(puthash "prevAll" (quote (("name" . "prevAll") ("signatures" "prevAll" (("selector" "A string containing a selector expression to match elements against.

" "true" nil))) ("desc" (text . "Get all preceding siblings of each element in the set of matched
elements, optionally filtered by a selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.prevAll() method searches through the predecessors of these elements
in the DOM tree and construct a new jQuery object from the matching
elements; the elements are returned in order beginning with the closest
sibling.
") (text . "") (text . "The method optionally accepts a selector expression of the same type
that we can pass to the $() function. If the selector is supplied, the
elements will be filtered by testing whether they match it.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
    <ul>
    <li>list item 1</li>
    <li>list item 2</li>
    <li class=\"third-item\">list item 3</li>
    <li>list item 4</li>
    <li>list item 5</li>
    </ul>
  ") (text . "") (text . "If we begin at the third item, we can find the elements which come
before it:

") (text . "") (js . "$('li.third-item').prevAll().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind items 1 and 2. Since
we do not supply a selector expression, these preceding elements are
unequivocally included as part of the object. If we had supplied one,
the elements would be tested for a match before they were included.
")) ("examples" ((text . "") (text . "Locate all the divs preceding the last div and give them a class.

") (text . "") (js . "$(\"div:last\").prevAll().addClass(\"before\");") (text . "") (css . "
    
    div { width:70px; height:70px; background:#abc; 
    border:2px solid black; margin:10px; float:left; }
    div.before { border-color: red; }
    ") (text . "") (html . "<div></div>
    <div></div>
    <div></div>
    <div></div>") (text . ""))))) jquery-doc-hash)

(push "prev" jquery-methods)

(puthash "prev" (quote (("name" . "prev") ("signatures" "prev" (("selector" "A string containing a selector expression to match elements against.

" "true" nil))) ("desc" (text . "Get the immediately preceding sibling of each element in the set of
matched elements, optionally filtered by a selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.prev() method allows us to search through the predecessors of these
elements in the DOM tree and construct a new jQuery object from the
matching elements.
") (text . "") (text . "The method optionally accepts a selector expression of the same type
that we can pass to the $() function. If the selector is supplied, the
elements will be filtered by testing whether they match it.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
    <ul>
    <li>list item 1</li>
    <li>list item 2</li>
    <li class=\"third-item\">list item 3</li>
    <li>list item 4</li>
    <li>list item 5</li>
    </ul>
  ") (text . "") (text . "If we begin at the third item, we can find the element which comes just
before it:

") (text . "") (js . "$('li.third-item').prev().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind item 2. Since we do
not supply a selector expression, this preceding element is
unequivocally included as part of the object. If we had supplied one,
the element would be tested for a match before it was included.
")) ("examples" ((text . "") (text . "Find the very previous sibling of each div.

") (text . "") (js . "
    var $curr = $(\"#start\");
    $curr.css(\"background\", \"#f99\");
    $(\"button\").click(function () {
    $curr = $curr.prev();
    $(\"div\").css(\"background\", \"\");
    $curr.css(\"background\", \"#f99\");
    });
    
    ") (text . "") (css . "
    div { width:40px; height:40px; margin:10px;
    float:left; border:2px blue solid; 
    padding:2px; }
    span { font-size:14px; }
    p { clear:left; margin:10px; }
    ") (text . "") (html . "<div></div>
    <div></div>
    <div><span>has child</span></div>
    
    <div></div>
    <div></div>
    <div></div>
    <div id=\"start\"></div>
    
    <div></div>
    <p><button>Go to Prev</button></p>") (text . "")) ((text . "") (text . "For each paragraph, find the very previous sibling that has a class
\"selected\".

") (text . "") (js . "$(\"p\").prev(\".selected\").css(\"background\", \"yellow\");") (text . "") (html . "<div><span>Hello</span></div>
    
    <p class=\"selected\">Hello Again</p>
    <p>And Again</p>") (text . ""))))) jquery-doc-hash)

(push "fadeTo" jquery-methods)

(puthash "fadeTo" (quote (("name" . "fadeTo") ("signatures" "fadeTo" (("duration" "A string or number determining how long the animation will run.

" nil nil) ("opacity" "A number between 0 and 1 denoting the target opacity.

" nil nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" nil nil) ("opacity" "A number between 0 and 1 denoting the target opacity.

" nil nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Adjust the opacity of the matched elements.

")) ("longdesc" (text . "") (text . "The .fadeTo() method animates the opacity of the matched elements.


") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively. If any other string is supplied, the default duration of
400 milliseconds is used. Unlike the other effect methods, .fadeTo()
requires that duration be explicitly specified.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
    Click here
    </div>
    <img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />
    With the element initially shown, we can dim it slowly:
    $('#clickme').click(function() {
    $('#book').fadeTo('slow', 0.5, function() {
    // Animation complete.
    });
    });
    ") (text . "") (text . "

") (text . "") (text . "With duration set to 0, this method just changes the opacity CSS
property, so .fadeTo(0, opacity) is the same as .css(ʼopacityʼ,
opacity).
") (text . "")) ("examples" ((text . "") (text . "Animates first paragraph to fade to an opacity of 0.33 (33%, about one
third visible), completing the animation within 600 milliseconds.

") (text . "") (js . "
    $(\"p:first\").click(function () {
    $(this).fadeTo(\"slow\", 0.33);
    });
    ") (text . "") (html . "<p>
    Click this paragraph to see it fade.
    </p>

    <p>
    Compare to this one that won't fade.
    </p>") (text . "")) ((text . "") (text . "Fade div to a random opacity on each click, completing the animation
within 200 milliseconds.

") (text . "") (js . "
    $(\"div\").click(function () {
    $(this).fadeTo(\"fast\", Math.random());
    });
    ") (text . "") (css . "
    p { width:80px; margin:0; padding:5px; }
    div { width:40px; height:40px; position:absolute; }
    div#one { top:0; left:0; background:#f00; }
    div#two { top:20px; left:20px; background:#0f0; }
    div#three { top:40px; left:40px; background:#00f; }
    ") (text . "") (html . "<p>And this is the library that John built...</p>

    <div id=\"one\"></div>
    <div id=\"two\"></div>
    <div id=\"three\"></div>") (text . "")) ((text . "") (text . "Find the right answer! The fade will take 250 milliseconds and change
various styles when it completes.

") (text . "") (js . "
    var getPos = function (n) {
    return (Math.floor(n) * 90) + \"px\";
    };
    $(\"p\").each(function (n) {
    var r = Math.floor(Math.random() * 3);
    var tmp = $(this).text();
    $(this).text($(\"p:eq(\" + r + \")\").text());
    $(\"p:eq(\" + r + \")\").text(tmp);
    $(this).css(\"left\", getPos(n));
    });
    $(\"div\").each(function (n) {
    $(this).css(\"left\", getPos(n));
    })
    .css(\"cursor\", \"pointer\")
    .click(function () {
    $(this).fadeTo(250, 0.25, function () {
    $(this).css(\"cursor\", \"\")
    .prev().css({\"font-weight\": \"bolder\",
    \"font-style\": \"italic\"});
    });
    });

    ") (text . "") (css . "
    div, p { width:80px; height:40px; top:0; margin:0; 
    position:absolute; padding-top:8px; }
    p { background:#fcc; text-align:center; }
    div { background:blue; }
    ") (text . "") (html . "<p>Wrong</p>
    <div></div>
    <p>Wrong</p>
    <div></div>

    <p>Right!</p>
    <div></div>") (text . ""))))) jquery-doc-hash)

(push "fadeOut" jquery-methods)

(puthash "fadeOut" (quote (("name" . "fadeOut") ("signatures" "fadeOut" (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Hide the matched elements by fading them to transparent.

")) ("longdesc" (text . "") (text . "The .fadeOut() method animates the opacity of the matched elements.
Once the opacity reaches 0, the display style property is set to none,
so the element no longer affects the layout of the page.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively. If any other string is supplied, or if the duration
parameter is omitted, the default duration of 400 milliseconds is used.
") (text . "") (text . "As of jQuery 1.4.3, an optional string naming an easing function may be
used. Easing functions specify the speed at which the animation
progresses at different points within the animation. The only easing
implementations in the jQuery library are the default, called swing,
and one that progresses at a constant pace, called linear. More easing
functions are available with the use of plug-ins, most notably the
jQuery UI suite.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
      Click here
      </div>
      <img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />") (text . "") (text . "With the element initially shown, we can hide it slowly:


") (text . "") (js . "$('#clickme').click(function() {
      $('#book').fadeOut('slow', function() {
      // Animation complete.
      });
      });") (text . "") (text . "

") (text . "")) ("examples" ((text . "") (text . "Animates all paragraphs to fade out, completing the animation within
600 milliseconds.

") (text . "") (js . "
      $(\"p\").click(function () {
      $(\"p\").fadeOut(\"slow\");
      });
      ") (text . "") (css . "
      p { font-size:150%; cursor:pointer; }
      ") (text . "") (html . "<p>
      If you click on this paragraph
      you'll see it just fade away.
      </p>") (text . "")) ((text . "") (text . "Fades out spans in one section that you click on.

") (text . "") (js . "
      
      $(\"span\").click(function () {
      $(this).fadeOut(1000, function () {
      $(\"div\").text(\"'\" + $(this).text() + \"' has faded!\");
      $(this).remove();
      });
      });
      $(\"span\").hover(function () {
      $(this).addClass(\"hilite\");
      }, function () {
      $(this).removeClass(\"hilite\");
      });
      
      ") (text . "") (css . "
      span { cursor:pointer; }
      span.hilite { background:yellow; }
      div { display:inline; color:red; }
      ") (text . "") (html . "<h3>Find the modifiers - <div></div></h3>
      <p>
      If you <span>really</span> want to go outside
      <span>in the cold</span> then make sure to wear
      your <span>warm</span> jacket given to you by
      your <span>favorite</span> teacher.
      </p>") (text . "")) ((text . "") (text . "Fades out two divs, one with a \"linear\" easing and one with the
default, \"swing,\" easing.

") (text . "") (js . "
      $(\"#btn1\").click(function() {
      function complete() {
      $(\"<div/>\").text(this.id).appendTo(\"#log\");
      }
      
      $(\"#box1\").fadeOut(1600, \"linear\", complete);
      $(\"#box2\").fadeOut(1600, complete);
      });
      
      $(\"#btn2\").click(function() {
      $(\"div\").show();
      $(\"#log\").empty();
      });
      
      ") (text . "") (css . "
      .box,
      button { float:left; margin:5px 10px 5px 0; }
      .box { height:80px; width:80px; background:#090; }
      #log { clear:left; }
      
      ") (text . "") (html . "
      <button id=\"btn1\">fade out</button>
      <button id=\"btn2\">show</button>
      
      <div id=\"log\"></div>
      
      <div id=\"box1\" class=\"box\">linear</div>
      <div id=\"box2\" class=\"box\">swing</div>
      ") (text . ""))))) jquery-doc-hash)

(push "parents" jquery-methods)

(puthash "parents" (quote (("name" . "parents") ("signatures" "parents" (("selector" "A string containing a selector expression to match elements against.

" "true" nil))) ("desc" (text . "Get the ancestors of each element in the current set of matched
elements, optionally filtered by a selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.parents() method allows us to search through the ancestors of these
elements in the DOM tree and construct a new jQuery object from the
matching elements ordered from immediate parent on up; the elements are
returned in order from the closest parent to the outer ones. The
.parents() and .parent() methods are similar, except that the latter
only travels a single level up the DOM tree.
") (text . "") (text . "The method optionally accepts a selector expression of the same type
that we can pass to the $() function. If the selector is supplied, the
elements will be filtered by testing whether they match it.
") (text . "") (text . "Consider a page with a basic nested list on it:


") (text . "") (js . "
	<ul class=\"level-1\">
	<li class=\"item-i\">I</li>
	<li class=\"item-ii\">II
	<ul class=\"level-2\">
	<li class=\"item-a\">A</li>
	<li class=\"item-b\">B
	<ul class=\"level-3\">
	<li class=\"item-1\">1</li>
	<li class=\"item-2\">2</li>
	<li class=\"item-3\">3</li>
	</ul>
	</li>
	<li class=\"item-c\">C</li>
	</ul>
	</li>
	<li class=\"item-iii\">III</li>
	</ul>
      ") (text . "") (text . "If we begin at item A, we can find its ancestors:


") (text . "") (js . "$('li.item-a').parents().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background for the level-2 list, item
II, and the level-1 list (and on up the DOM tree all the way to the
<html> element). Since we do not supply a selector expression, all of
the ancestors are part of the returned jQuery object. If we had
supplied one, only the matching items among these would be included.
")) ("examples" ((text . "") (text . "Find all parent elements of each b.

") (text . "") (js . "
	var parentEls = $(\"b\").parents()
	.map(function () { 
	return this.tagName; 
	})
	.get().join(\", \");
	$(\"b\").append(\"<strong>\" + parentEls + \"</strong>\");
	
	") (text . "") (css . "
	b, span, p, html body {
	padding: .5em;
	border: 1px solid;
	}
	b { color:blue; }
	strong { color:red; }
	") (text . "") (html . "<div>
	<p>
	<span>
	<b>My parents are: </b>
	</span>
	
	</p>
	</div>") (text . "")) ((text . "") (text . "Click to find all unique div parent elements of each span.

") (text . "") (js . "
	function showParents() {
	$(\"div\").css(\"border-color\", \"white\");
	var len = $(\"span.selected\")
	.parents(\"div\")
	.css(\"border\", \"2px red solid\")
	.length;
	$(\"b\").text(\"Unique div parents: \" + len);
	}
	$(\"span\").click(function () {
	$(this).toggleClass(\"selected\");
	showParents();
	});") (text . "") (css . "
	
	p, div, span {margin:2px; padding:1px; }
	div { border:2px white solid; }
	span { cursor:pointer; font-size:12px; }
	.selected { color:blue; }
	b { color:red; display:block; font-size:14px; }
	") (text . "") (html . "<p>
	<div>
	<div><span>Hello</span></div>
	<span>Hello Again</span>
	
	</div>
	<div>
	<span>And Hello Again</span>
	</div>
	</p>
	
	<b>Click Hellos to toggle their parents.</b>") (text . ""))))) jquery-doc-hash)

(push "fadeIn" jquery-methods)

(puthash "fadeIn" (quote (("name" . "fadeIn") ("signatures" "fadeIn" (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Display the matched elements by fading them to opaque.

")) ("longdesc" (text . "") (text . "The .fadeIn() method animates the opacity of the matched elements.


") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively. If any other string is supplied, or if the duration
parameter is omitted, the default duration of 400 milliseconds is used.
") (text . "") (text . "As of jQuery 1.4.3, an optional string naming an easing function may be
used. Easing functions specify the speed at which the animation
progresses at different points within the animation. The only easing
implementations in the jQuery library are the default, called swing,
and one that progresses at a constant pace, called linear. More easing
functions are available with the use of plug-ins, most notably the
jQuery UI suite.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
	Click here
	</div>
	<img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />
	With the element initially hidden, we can show it slowly:
	$('#clickme').click(function() {
	$('#book').fadeIn('slow', function() {
	// Animation complete
	});
	});") (text . "") (text . "

") (text . "")) ("examples" ((text . "") (text . "Animates hidden divs to fade in one by one, completing each animation
within 600 milliseconds.

") (text . "") (js . "
	$(document.body).click(function () {
	$(\"div:hidden:first\").fadeIn(\"slow\");
	});
	") (text . "") (css . "
	span { color:red; cursor:pointer; }
	div { margin:3px; width:80px; display:none;
	height:80px; float:left; }
	div#one { background:#f00; }
	div#two { background:#0f0; }
	div#three { background:#00f; }
	") (text . "") (html . "<span>Click here...</span>
	
	<div id=\"one\"></div>
	<div id=\"two\"></div>
	<div id=\"three\"></div>") (text . "")) ((text . "") (text . "Fades a red block in over the text. Once the animation is done, it
quickly fades in more text on top.

") (text . "") (js . "
	$(\"a\").click(function () {
	$(\"div\").fadeIn(3000, function () {
	$(\"span\").fadeIn(100);
	});
	return false;
	}); 
	
	") (text . "") (css . "
	p { position:relative; width:400px; height:90px; }
	div { position:absolute; width:400px; height:65px; 
	font-size:36px; text-align:center; 
	color:yellow; background:red;
	padding-top:25px; 
	top:0; left:0; display:none; }
	span { display:none; }
	") (text . "") (html . "<p>
	Let it be known that the party of the first part
	and the party of the second part are henceforth
	and hereto directed to assess the allegations
	for factual correctness... (<a href=\"#\">click!</a>)
	<div><span>CENSORED!</span></div>
	
	</p>") (text . ""))))) jquery-doc-hash)

(push "parent" jquery-methods)

(puthash "parent" (quote (("name" . "parent") ("signatures" "parent" (("selector" "A string containing a selector expression to match elements against.

" "true" nil))) ("desc" (text . "Get the parent of each element in the current set of matched elements,
optionally filtered by a selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.parent() method allows us to search through the parents of these
elements in the DOM tree and construct a new jQuery object from the
matching elements. The .parents() and .parent() methods are similar,
except that the latter only travels a single level up the DOM tree.
") (text . "") (text . "The method optionally accepts a selector expression of the same type
that we can pass to the $() function. If the selector is supplied, the
elements will be filtered by testing whether they match it.
") (text . "") (text . "Consider a page with a basic nested list on it:


") (text . "") (js . "
	  <ul class=\"level-1\">
	  <li class=\"item-i\">I</li>
	  <li class=\"item-ii\">II
	  <ul class=\"level-2\">
	  <li class=\"item-a\">A</li>
	  <li class=\"item-b\">B
          <ul class=\"level-3\">
          <li class=\"item-1\">1</li>
          <li class=\"item-2\">2</li>
          <li class=\"item-3\">3</li>
          </ul>
	  </li>
	  <li class=\"item-c\">C</li>
	  </ul>
	  </li>
	  <li class=\"item-iii\">III</li>
	  </ul>
	") (text . "") (text . "If we begin at item A, we can find its parents:


") (text . "") (js . "$('li.item-a').parent().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background for the level-2 list. Since
we do not supply a selector expression, the parent element is
unequivocally included as part of the object. If we had supplied one,
the element would be tested for a match before it was included.
")) ("examples" ((text . "") (text . "Shows the parent of each element as (parent > child). Check the View
Source to see the raw html.

") (text . "") (js . "
	  
	  $(\"*\", document.body).each(function () {
	  var parentTag = $(this).parent().get(0).tagName;
	  $(this).prepend(document.createTextNode(parentTag + \" > \"));
	  });
	  ") (text . "") (css . "
	  div,p { margin:10px; }
	  ") (text . "") (html . "<div>div, 
	  <span>span, </span>
	  <b>b </b>
	  
	  </div>
	  <p>p, 
	  <span>span, 
	  <em>em </em>
	  </span>
	  </p>
	  
	  <div>div, 
	  <strong>strong, 
	  <span>span, </span>
	  <em>em, 
          <b>b, </b>
	  </em>
	  
	  </strong>
	  <b>b </b>
	  </div>") (text . "")) ((text . "") (text . "Find the parent element of each paragraph with a class \"selected\".

") (text . "") (js . "$(\"p\").parent(\".selected\").css(\"background\", \"yellow\");") (text . "") (html . "<div><p>Hello</p></div>
	  
	  <div class=\"selected\"><p>Hello Again</p></div>
	  ") (text . ""))))) jquery-doc-hash)

(push "offsetParent" jquery-methods)

(puthash "offsetParent" (quote (("name" . "offsetParent") ("signatures" "offsetParent" nil) ("desc" (text . "Get the closest ancestor element that is positioned.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.offsetParent() method allows us to search through the ancestors of
these elements in the DOM tree and construct a new jQuery object
wrapped around the closest positioned ancestor. An element is said to
be positioned if it has a CSS position attribute of relative, absolute,
or fixed. This information is useful for calculating offsets for
performing animations and placing objects on the page.
") (text . "") (text . "Consider a page with a basic nested list on it, with a positioned
element:

") (text . "") (js . "
	  <ul class=\"level-1\">
	  <li class=\"item-i\">I</li>
	  <li class=\"item-ii\" style=\"position: relative;\">II
	  <ul class=\"level-2\">
	  <li class=\"item-a\">A</li>
	  <li class=\"item-b\">B
          <ul class=\"level-3\">
          <li class=\"item-1\">1</li>
          <li class=\"item-2\">2</li>
          <li class=\"item-3\">3</li>
          </ul>
	  </li>
	  <li class=\"item-c\">C</li>
	  </ul>
	  </li>
	  <li class=\"item-iii\">III</li>
	  </ul>
	") (text . "") (text . "If we begin at item A, we can find its positioned ancestor:


") (text . "") (js . "$('li.item-a').offsetParent().css('background-color', 'red');") (text . "") (text . "This will change the color of list item II, which is positioned.


") (text . "")) ("examples" ((text . "") (text . "Find the offsetParent of item \"A.\"

") (text . "") (text . "250

") (text . "") (js . "$('li.item-a').offsetParent().css('background-color', 'red');") (text . "") (html . "
	  <ul class=\"level-1\">
	  <li class=\"item-i\">I</li>
	  <li class=\"item-ii\" style=\"position: relative;\">II
	  <ul class=\"level-2\">
	  <li class=\"item-a\">A</li>
	  <li class=\"item-b\">B
	  <ul class=\"level-3\">
	  <li class=\"item-1\">1</li>
	  <li class=\"item-2\">2</li>
	  <li class=\"item-3\">3</li>
	  </ul>
	  </li>
	  <li class=\"item-c\">C</li>
	  </ul>
	  </li>
	  <li class=\"item-iii\">III</li>
	  </ul>
	  ") (text . ""))))) jquery-doc-hash)

(push "slideToggle" jquery-methods)

(puthash "slideToggle" (quote (("name" . "slideToggle") ("signatures" "slideToggle" (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Display or hide the matched elements with a sliding motion.

")) ("longdesc" (text . "") (text . "The .slideToggle() method animates the height of the matched elements.
This causes lower parts of the page to slide up or down, appearing to
reveal or conceal the items. If the element is initially displayed, it
will be hidden; if hidden, it will be shown. The display property is
saved and restored as needed. If an element has a display value of
inline, then is hidden and shown, it will once again be displayed
inline. When the height reaches 0 after a hiding animation, the display
style property is set to none to ensure that the element no longer
affects the layout of the page.
") (text . "") (text . "As of jQuery 1.4.3, an optional string naming an easing function may be
used. Easing functions specify the speed at which the animation
progresses at different points within the animation. The only easing
implementations in the jQuery library are the default, called swing,
and one that progresses at a constant pace, called linear. More easing
functions are available with the use of plug-ins, most notably the
jQuery UI suite.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
	  Click here
	  </div>
	  <img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />") (text . "") (text . "We will cause .slideToggle() to be called when another element is
clicked:

") (text . "") (js . "$('#clickme').click(function() {
	  $('#book').slideToggle('slow', function() {
	  // Animation complete.
	  });
	  });
	  ") (text . "") (text . "With the element initially shown, we can hide it slowly with the first
click:

") (text . "") (text . "

") (text . "") (text . "A second click will show the element once again:


") (text . "") (text . "

") (text . "")) ("examples" ((text . "") (text . "Animates all paragraphs to slide up or down, completing the animation
within 600 milliseconds.

") (text . "") (js . "
	  $(\"button\").click(function () {
	  $(\"p\").slideToggle(\"slow\");
	  });
	  ") (text . "") (css . "
	  p { width:400px; }
	  ") (text . "") (html . "<button>Toggle</button>
	  
	  <p>
	  This is the paragraph to end all paragraphs.  You
	  should feel <em>lucky</em> to have seen such a paragraph in
	  your life.  Congratulations!
	  </p>") (text . "")) ((text . "") (text . "Animates divs between dividers with a toggle that makes some appear and
some disappear.

") (text . "") (js . "
	  $(\"#aa\").click(function () {
	  $(\"div:not(.still)\").slideToggle(\"slow\", function () {
	  var n = parseInt($(\"span\").text(), 10);
	  $(\"span\").text(n + 1);
	  });
	  });
	  
	  ") (text . "") (css . "
	  div { background:#b977d1; margin:3px; width:60px; 
	  height:60px; float:left; }
	  div.still { background:#345; width:5px; }
	  div.hider { display:none; }
	  span { color:red; }
	  p { clear: left; }") (text . "") (html . "<div></div>
	  <div class=\"still\"></div>
	  <div style=\"display:none;\">
	  </div><div class=\"still\"></div>
	  <div></div>
	  <div class=\"still\"></div>
	  <div class=\"hider\"></div>
	  <div class=\"still\"></div>
	  <div class=\"hider\"></div>
	  <div class=\"still\"></div>
	  <div></div>
	  <p><button id=\"aa\">Toggle</button> There have been <span>0</span> toggled divs.</p>") (text . ""))))) jquery-doc-hash)

(push "$.post" jquery-methods)

(puthash "$.post" (quote (("name" . "$.post") ("signatures" "$.post" (("url" "A string containing the URL to which the request is sent.

" nil nil) ("data" "A map or string that is sent to the server with the request.

" "true" nil) ("success(data, textStatus, jqXHR)" "A callback function that is executed if the request succeeds.

" "true" nil) ("dataType" "The type of data expected from the server. Default: Intelligent Guess
(xml, json, script, or html).

" "true" nil))) ("desc" (text . "Load data from the server using a HTTP POST request.

")) ("longdesc" (text . "This is a shorthand Ajax function, which is equivalent to:


") (text . "") (js . "$.ajax({
	  type: 'POST',
	  url: ") (text . "") (text . "The success callback function is passed the returned data, which will
be an XML root element or a text string depending on the MIME type of
the response. It is also passed the text status of the response.
") (text . "") (text . "As of jQuery 1.5, the success callback function is also passed a
\"jqXHR\" object (in jQuery 1.4, it was passed the XMLHttpRequest
object).
") (text . "") (text . "Most implementations will specify a success handler:


") (text . "") (js . "$.post('ajax/test.html', function(data) {
	  $('.result').html(data);
	  });
	  ") (text . "") (text . "This example fetches the requested HTML snippet and inserts it on the
page.

") (text . "") (text . "Pages fetched with POST are never cached, so the cache and ifModified
options in jQuery.ajaxSetup() have no effect on these requests.

") (text . "") (text . " The jqXHR Object


") (text . "") (text . "As of jQuery 1.5, all of jQueryʼs Ajax methods return a superset of the
XMLHTTPRequest object. This jQuery XHR object, or \"jqXHR,\" returned by
$.post() implements the Promise interface, giving it all the
properties, methods, and behavior of a Promise (see Deferred object for
more information). For convenience and consistency with the callback
names used by $.ajax() , it provides .error(), .success(), and
.complete() methods. These methods take a function argument that is
called when the request terminates, and the function receives the same
arguments as the correspondingly-named $.ajax() callback.
") (text . "") (text . "The Promise interface in jQuery 1.5 also allows jQueryʼs Ajax methods,
including $.post(), to chain multiple .success(), .complete(), and
.error() callbacks on a single request, and even to assign these
callbacks after the request may have completed. If the request is
already complete, the callback is fired immediately.
") (text . "") (js . "// Assign handlers immediately after making the request,
	  // and remember the jqxhr object for this request
	  var jqxhr = $.post(\"example.php\", function() {
	  alert(\"success\");
	  })
	  .success(function() { alert(\"second success\"); })
	  .error(function() { alert(\"error\"); })
	  .complete(function() { alert(\"complete\"); });

	  // perform other work here ...

	  // Set another completion function for the request above
	  jqxhr.complete(function(){ alert(\"second complete\"); });") (text . "")) ("examples" ((text . "") (text . "Request the test.php page, but ignore the return results.

") (text . "") (js . "$.post(\"test.php\");") (text . "")) ((text . "") (text . "Request the test.php page and send some additional data along (while
still ignoring the return results).

") (text . "") (js . "$.post(\"test.php\", { name: \"John\", time: \"2pm\" } );") (text . "")) ((text . "") (text . "pass arrays of data to the server (while still ignoring the return
results).

") (text . "") (js . "$.post(\"test.php\", { 'choices[]': [\"Jon\", \"Susan\"] });") (text . "")) ((text . "") (text . "send form data using ajax requests

") (text . "") (js . "$.post(\"test.php\", $(\"#testform\").serialize());") (text . "")) ((text . "") (text . "Alert out the results from requesting test.php (HTML or XML, depending
on what was returned).

") (text . "") (js . "$.post(\"test.php\", function(data) {
	    alert(\"Data Loaded: \" + data);
	    });") (text . "")) ((text . "") (text . "Alert out the results from requesting test.php with an additional
payload of data (HTML or XML, depending on what was returned).

") (text . "") (js . "$.post(\"test.php\", { name: \"John\", time: \"2pm\" },
	    function(data) {
	    alert(\"Data Loaded: \" + data);
	    });") (text . "")) ((text . "") (text . "Gets the test.php page content, store it in a XMLHttpResponse object
and applies the process() JavaScript function.

") (text . "") (js . "$.post(\"test.php\", { name: \"John\", time: \"2pm\" },
	    function(data) {
	    process(data);
	    }, 
	    \"xml\"
	    );") (text . "")) ((text . "") (text . "Posts to the test.php page and gets contents which has been returned in
json format (<?php echo
json_encode(array(\"name\"=>\"John\",\"time\"=>\"2pm\")); ?>).
") (text . "") (js . "$.post(\"test.php\", { \"func\": \"getNameAndTime\" },
	    function(data){
	    alert(data.name); // John
	    console.log(data.time); //  2pm
	    }, \"json\");") (text . "")) ((text . "") (text . "Post a form using ajax and put results in a div

") (text . "") (js . "
	    // attach a submit handler to the form
	    $(\"#searchForm\").submit(function(event) {

	    // stop form from submitting normally
	    event.preventDefault(); 
	    
	    // get some values from elements on the page:
	    var $form = $( this ),
	    term = $form.find( 'input[name=\"s\"]' ).val(),
	    url = $form.attr( 'action' );

	    // Send the data using post and put the results in a div
	    $.post( url, { s: term } ,
	    function( data ) {
	    var content = $( data ).find( '#content' );
	    $( \"#result\" ).html( content );
	    }
	    );
	    });
	    ") (text . "") (html . "<form action=\"/\" id=\"searchForm\">
	    <input type=\"text\" name=\"s\" placeholder=\"Search...\" />
	    <input type=\"submit\" value=\"Search\" />
	    </form>
	    <!-- the result of the search will be rendered inside this div -->
	    <div id=\"result\"></div>
	    ") (text . ""))))) jquery-doc-hash)

(push "slideUp" jquery-methods)

(puthash "slideUp" (quote (("name" . "slideUp") ("signatures" "slideUp" (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Hide the matched elements with a sliding motion.

")) ("longdesc" (text . "") (text . "The .slideUp() method animates the height of the matched elements. This
causes lower parts of the page to slide up, appearing to conceal the
items. Once the height reaches 0, the display style property is set to
none to ensure that the element no longer affects the layout of the
page.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively. If any other string is supplied, or if the duration
parameter is omitted, the default duration of 400 milliseconds is used.
") (text . "") (text . "As of jQuery 1.4.3, an optional string naming an easing function may be
used. Easing functions specify the speed at which the animation
progresses at different points within the animation. The only easing
implementations in the jQuery library are the default, called swing,
and one that progresses at a constant pace, called linear. More easing
functions are available with the use of plug-ins, most notably the
jQuery UI suite.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
	      Click here
	      </div>
	      <img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />") (text . "") (text . "With the element initially shown, we can hide it slowly:


") (text . "") (js . "$('#clickme').click(function() {
	      $('#book').slideUp('slow', function() {
	      // Animation complete.
	      });
	      });
	      ") (text . "") (text . "

") (text . "")) ("examples" ((text . "") (text . "Animates all divs to slide up, completing the animation within 400
milliseconds.

") (text . "") (js . "
	      $(document.body).click(function () {
	      if ($(\"div:first\").is(\":hidden\")) {
	      $(\"div\").show(\"slow\");
	      } else {
	      $(\"div\").slideUp();
	      }
	      });

	      ") (text . "") (css . "
	      div { background:#3d9a44; margin:3px; width:80px; 
	      height:40px; float:left; }
	      ") (text . "") (html . "Click me!
	      <div></div>
	      <div></div>
	      <div></div>
	      <div></div>

	      <div></div>") (text . "")) ((text . "") (text . "Animates the parent paragraph to slide up, completing the animation
within 200 milliseconds. Once the animation is done, it displays an
alert.
") (text . "") (js . "
	      $(\"button\").click(function () {
	      $(this).parent().slideUp(\"slow\", function () {
	      $(\"#msg\").text($(\"button\", this).text() + \" has completed.\");
	      });
	      });

	      ") (text . "") (css . "
	      div { margin:2px; }
	      ") (text . "") (html . "<div>
	      <button>Hide One</button>
	      <input type=\"text\" value=\"One\" />

	      </div>
	      <div>
	      <button>Hide Two</button>
	      <input type=\"text\" value=\"Two\" />

	      </div>
	      <div>
	      <button>Hide Three</button>
	      <input type=\"text\" value=\"Three\" />

	      </div>
	      <div id=\"msg\"></div>") (text . ""))))) jquery-doc-hash)

(push "nextAll" jquery-methods)

(puthash "nextAll" (quote (("name" . "nextAll") ("signatures" "nextAll" (("selector" "A string containing a selector expression to match elements against.

" "true" nil))) ("desc" (text . "Get all following siblings of each element in the set of matched
elements, optionally filtered by a selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.nextAll() method allows us to search through the successors of these
elements in the DOM tree and construct a new jQuery object from the
matching elements.
") (text . "") (text . "The method optionally accepts a selector expression of the same type
that we can pass to the $() function. If the selector is supplied, the
elements will be filtered by testing whether they match it.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
		<ul>
		<li>list item 1</li>
		<li>list item 2</li>
		<li class=\"third-item\">list item 3</li>
		<li>list item 4</li>
		<li>list item 5</li>
		</ul>
	      ") (text . "") (text . "If we begin at the third item, we can find the elements which come
after it:

") (text . "") (js . "$('li.third-item').nextAll().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind items 4 and 5. Since
we do not supply a selector expression, these following elements are
unequivocally included as part of the object. If we had supplied one,
the elements would be tested for a match before they were included.
")) ("examples" ((text . "") (text . "Locate all the divs after the first and give them a class.

") (text . "") (js . "$(\"div:first\").nextAll().addClass(\"after\");") (text . "") (css . "
		
		div { width: 80px; height: 80px; background: #abc; 
		border: 2px solid black; margin: 10px; float: left; }
		div.after { border-color: red; }
		") (text . "") (text . "150

") (text . "") (html . "<div>first</div>
		<div>sibling<div>child</div></div>
		<div>sibling</div>
		
		<div>sibling</div>") (text . "")) ((text . "") (text . "Locate all the paragraphs after the second child in the body and give
them a class.

") (text . "") (js . "
		$(\":nth-child(1)\").nextAll(\"p\").addClass(\"after\");
		") (text . "") (css . "
		div, p { width: 60px; height: 60px; background: #abc;
		border: 2px solid black; margin: 10px; float: left; }
		.after { border-color: red; }
		") (text . "") (text . "200

") (text . "") (html . "<p>p</p>
		
		<div>div</div>
		<p>p</p>
		<p>p</p>
		<div>div</div>
		
		<p>p</p>
		<div>div</div>") (text . ""))))) jquery-doc-hash)

(push "next" jquery-methods)

(puthash "next" (quote (("name" . "next") ("signatures" "next" (("selector" "A string containing a selector expression to match elements against.

" "true" nil))) ("desc" (text . "Get the immediately following sibling of each element in the set of
matched elements. If a selector is provided, it retrieves the next
sibling only if it matches that selector.
")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.next() method allows us to search through the immediately following
sibling of these elements in the DOM tree and construct a new jQuery
object from the matching elements.
") (text . "") (text . "The method optionally accepts a selector expression of the same type
that we can pass to the $() function. If the immediately following
sibling matches the selector, it remains in the newly constructed
jQuery object; otherwise, it is excluded.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
		<ul>
		<li>list item 1</li>
		<li>list item 2</li>
		<li class=\"third-item\">list item 3</li>
		<li>list item 4</li>
		<li>list item 5</li>
		</ul>
	      ") (text . "") (text . "If we begin at the third item, we can find the element which comes just
after it:

") (text . "") (js . "$('li.third-item').next().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind item 4. Since we do
not supply a selector expression, this following element is
unequivocally included as part of the object. If we had supplied one,
the element would be tested for a match before it was included.
")) ("examples" ((text . "") (text . "Find the very next sibling of each disabled button and change its text
\"this button is disabled\".

") (text . "") (js . "$(\"button[disabled]\").next().text(\"this button is disabled\");") (text . "") (css . "
		
		span { color:blue; font-weight:bold; }
		button { width:100px; }
		") (text . "") (html . "<div><button disabled=\"disabled\">First</button> - <span></span></div>
		<div><button>Second</button> - <span></span></div>
		
		<div><button disabled=\"disabled\">Third</button> - <span></span></div>") (text . "")) ((text . "") (text . "Find the very next sibling of each paragraph. Keep only the ones with a
class \"selected\".

") (text . "") (js . "$(\"p\").next(\".selected\").css(\"background\", \"yellow\");") (text . "") (html . "<p>Hello</p>
		
		<p class=\"selected\">Hello Again</p>
		<div><span>And Again</span></div>") (text . ""))))) jquery-doc-hash)

(push "slideDown" jquery-methods)

(puthash "slideDown" (quote (("name" . "slideDown") ("signatures" "slideDown" (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Display the matched elements with a sliding motion.

")) ("longdesc" (text . "") (text . "The .slideDown() method animates the height of the matched elements.
This causes lower parts of the page to slide down, making way for the
revealed items.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively. If any other string is supplied, or if the duration
parameter is omitted, the default duration of 400 milliseconds is used.
") (text . "") (text . "As of jQuery 1.4.3, an optional string naming an easing function may be
used. Easing functions specify the speed at which the animation
progresses at different points within the animation. The only easing
implementations in the jQuery library are the default, called swing,
and one that progresses at a constant pace, called linear. More easing
functions are available with the use of plug-ins, most notably the
jQuery UI suite.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
		Click here
		</div>
		<img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />") (text . "") (text . "With the element initially hidden, we can show it slowly:


") (text . "") (js . "$('#clickme').click(function() {
		$('#book').slideDown('slow', function() {
		// Animation complete.
		});
		});") (text . "") (text . "

") (text . "")) ("examples" ((text . "") (text . "Animates all divs to slide down and show themselves over 600
milliseconds.

") (text . "") (js . "
		$(document.body).click(function () {
		if ($(\"div:first\").is(\":hidden\")) {
		$(\"div\").slideDown(\"slow\");
		} else {
		$(\"div\").hide();
		}
		});
		
		") (text . "") (css . "
		div { background:#de9a44; margin:3px; width:80px; 
		height:40px; display:none; float:left; }
		") (text . "") (html . "Click me!
		<div></div>
		<div></div>
		<div></div>") (text . "")) ((text . "") (text . "Animates all inputs to slide down, completing the animation within 1000
milliseconds. Once the animation is done, the input look is changed
especially if it is the middle input which gets the focus.
") (text . "") (js . "
		$(\"div\").click(function () {
		$(this).css({ borderStyle:\"inset\", cursor:\"wait\" });
		$(\"input\").slideDown(1000,function(){
		$(this).css(\"border\", \"2px red inset\")
		.filter(\".middle\")
		.css(\"background\", \"yellow\")
		.focus();
		$(\"div\").css(\"visibility\", \"hidden\");
		});
		});
		
		") (text . "") (css . "
		div { background:#cfd; margin:3px; width:50px; 
		text-align:center; float:left; cursor:pointer;
		border:2px outset black; font-weight:bolder; }
		input { display:none; width:120px; float:left; 
		margin:10px; }
		") (text . "") (html . "<div>Push!</div>
		<input type=\"text\" />
		<input type=\"text\" class=\"middle\" />
		
		<input type=\"text\" />") (text . ""))))) jquery-doc-hash)

(push "find" jquery-methods)

(puthash "find" (quote (("name" . "find") ("signatures" "find" (("selector" "A string containing a selector expression to match elements against.

" nil nil)) (("jQuery object" "A jQuery object to match elements against.

" nil nil)) (("element" "An element to match elements against.

" nil nil))) ("desc" (text . "Get the descendants of each element in the current set of matched
elements, filtered by a selector, jQuery object, or element.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.find() method allows us to search through the descendants of these
elements in the DOM tree and construct a new jQuery object from the
matching elements. The .find() and .children() methods are similar,
except that the latter only travels a single level down the DOM tree.
") (text . "") (text . "The first signature for the .find()method accepts a selector expression
of the same type that we can pass to the $() function. The elements
will be filtered by testing whether they match this selector.
") (text . "") (text . "Consider a page with a basic nested list on it:


") (text . "") (js . "
		  <ul class=\"level-1\">
		  <li class=\"item-i\">I</li>
		  <li class=\"item-ii\">II
		  <ul class=\"level-2\">
		  <li class=\"item-a\">A</li>
		  <li class=\"item-b\">B
		  <ul class=\"level-3\">
		  <li class=\"item-1\">1</li>
		  <li class=\"item-2\">2</li>
		  <li class=\"item-3\">3</li>
		  </ul>
		  </li>
		  <li class=\"item-c\">C</li>
		  </ul>
		  </li>
		  <li class=\"item-iii\">III</li>
		  </ul>
		") (text . "") (text . "If we begin at item II, we can find list items within it:


") (text . "") (js . "$('li.item-ii').find('li').css('background-color', 'red');") (text . "") (text . "The result of this call is a red background on items A, B, 1, 2, 3, and
C. Even though item II matches the selector expression, it is not
included in the results; only descendants are considered candidates for
the match.
") (text . "") (text . "  Unlike in the rest of the tree traversal methods, the selector
  expression is required in a call to .find(). If we need to retrieve
  all of the descendant elements, we can pass in the universal
  selector ʼ*ʼ to accomplish this.
") (text . "") (text . "Selector context is implemented with the .find() method; therefore,
$(ʼli.item-iiʼ).find(ʼliʼ) is equivalent to $(ʼliʼ, ʼli.item-iiʼ).

") (text . "") (text . "As of jQuery 1.6, we can also filter the selection with a given jQuery
collection or element. With the same nested list as above, if we start
with:
") (text . "") (js . "var $allListElements = $('li');") (text . "") (text . "And then pass this jQuery object to find:


") (text . "") (js . "$('li.item-ii').find( $allListElements );") (text . "") (text . "This will return a jQuery collection which contains only the list
elements that are descendants of item II.

") (text . "") (text . "Similarly, an element may also be passed to find:


") (text . "") (js . "
		  var item1 = $('li.item-1')[0];
		  $('li.item-ii').find( item1 ).css('background-color', 'red');
		") (text . "") (text . "The result of this call would be a red background on item 1.


") (text . "")) ("examples" ((text . "") (text . "Starts with all paragraphs and searches for descendant span elements,
same as $(\"p span\")

") (text . "") (js . "
		  $(\"p\").find(\"span\").css('color','red');
		  ") (text . "") (html . "<p><span>Hello</span>, how are you?</p>
		  <p>Me? I'm <span>good</span>.</p>") (text . "")) ((text . "") (text . "A selection using a jQuery collection of all span tags. Only spans
within p tags are changed to red while others are left blue.

") (text . "") (css . "
		  span { color: blue; }
		  ") (text . "") (js . "
		  var $spans = $('span');
		  $(\"p\").find( $spans ).css('color','red');
		  ") (text . "") (html . "<p><span>Hello</span>, how are you?</p>
		  <p>Me? I'm <span>good</span>.</p>
		  <div>Did you <span>eat</span> yet?</div>") (text . "")) ((text . "") (text . "Add spans around each word then add a hover and italicize words with
the letter t.

") (text . "") (js . "
		  var newText = $(\"p\").text().split(\" \").join(\"</span> <span>\");
		  newText = \"<span>\" + newText + \"</span>\";
		  
		  $(\"p\").html( newText )
		  .find('span')
		  .hover(function() { 
		  $(this).addClass(\"hilite\"); 
		  },
		  function() { $(this).removeClass(\"hilite\"); 
		  })
		  .end()
		  .find(\":contains('t')\")
		  .css({\"font-style\":\"italic\", \"font-weight\":\"bolder\"});
		  
		  ") (text . "") (css . "
		  p { font-size:20px; width:200px; cursor:default; 
		  color:blue; font-weight:bold; margin:0 10px; }
		  .hilite { background:yellow; }
		  ") (text . "") (html . "<p>
		  When the day is short
		  find that which matters to you
		  or stop believing
		  </p>") (text . ""))))) jquery-doc-hash)

(push "$.getScript" jquery-methods)

(puthash "$.getScript" (quote (("name" . "$.getScript") ("signatures" "$.getScript" (("url" "A string containing the URL to which the request is sent.

" nil nil) ("success(data, textStatus)" "A callback function that is executed if the request succeeds.

" "true" nil))) ("desc" (text . "Load a JavaScript file from the server using a GET HTTP request, then
execute it.

")) ("longdesc" (text . "This is a shorthand Ajax function, which is equivalent to:


") (text . "") (js . "$.ajax({
		url: ") (text . "") (text . "The callback is passed the returned JavaScript file. This is generally
not useful as the script will already have run at this point.

") (text . "") (text . "The script is executed in the global context, so it can refer to other
variables and use jQuery functions. Included scripts should have some
impact on the current page:
") (text . "") (js . "$('.result').html('<p>Lorem ipsum dolor sit amet.</p>');") (text . "") (text . "The script can then be included and run by referencing the file name:


") (text . "") (js . "$.getScript('ajax/test.js', function() {
		alert('Load was performed.');
		});")) ("examples" ((text . "") (text . "Load the official jQuery Color Animation plugin dynamically and bind
some color animations to occur once the new functionality is loaded.

") (text . "") (js . "$.getScript(\"http://dev.jquery.com/view/trunk/plugins/color/jquery.color.js\", function(){
		  $(\"#go\").click(function(){
		  $(\".block\").animate( { backgroundColor: 'pink' }, 1000)
		  .animate( { backgroundColor: 'blue' }, 1000);
		  });
		  });") (text . "") (html . "<button id=\"go\">&raquo; Run</button>

		  <div class=\"block\"></div>
		  ") (text . "") (css . ".block { 
		  background-color: blue; 
		  width: 150px; 
		  height: 70px;
		  margin: 10px; 
		  }") (text . "")) ((text . "") (text . "Load the test.js JavaScript file and execute it.

") (text . "") (js . "$.getScript(\"test.js\");") (text . "")) ((text . "") (text . "Load the test.js JavaScript file and execute it, displaying an alert
message when the execution is complete.

") (text . "") (js . "$.getScript(\"test.js\", function(){
		  alert(\"Script loaded and executed.\");
		  });") (text . ""))))) jquery-doc-hash)

(push "contents" jquery-methods)

(puthash "contents" (quote (("name" . "contents") ("signatures" "contents" nil) ("desc" (text . "Get the children of each element in the set of matched elements,
including text and comment nodes.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.contents() method allows us to search through the immediate children
of these elements in the DOM tree and construct a new jQuery object
from the matching elements. The .contents() and .children() methods are
similar, except that the former includes text nodes as well as HTML
elements in the resulting jQuery object.
") (text . "") (text . "The .contents() method can also be used to get the content document of
an iframe, if the iframe is on the same domain as the main page.

") (text . "") (text . "Consider a simple <div> with a number of text nodes, each of which is
separated by two line break elements ( <br />):

") (text . "") (js . "<div class=\"container\">
		Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed 
		do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
		<br /><br />
		Ut enim ad minim veniam, quis nostrud exercitation ullamco 
		laboris nisi ut aliquip ex ea commodo consequat.
		<br /> <br />
		Duis aute irure dolor in reprehenderit in voluptate velit 
		esse cillum dolore eu fugiat nulla pariatur.
		</div>
		") (text . "") (text . "We can employ the .contents() method to help convert this blob of text
into three well-formed paragraphs:

") (text . "") (js . "
		  $('.container').contents().filter(function() {
		  return this.nodeType == 3;
		  })
		  .wrap('<p></p>')
		  .end()
		  .filter('br')
		  .remove();
		") (text . "") (text . "This code first retrieves the contents of <div class=\"container\"> and
then filters it for text nodes, which are wrapped in paragraph tags.
This is accomplished by testing the .nodeType property of the element.
This DOM property holds a numeric code indicating the nodeʼs type; text
nodes use the code 3. The contents are again filtered, this time for
<br /> elements, and these elements are removed.
") (text . "")) ("examples" ((text . "") (text . "Find all the text nodes inside a paragraph and wrap them with a bold
tag.

") (text . "") (js . "$(\"p\").contents().filter(function(){ return this.nodeType != 1; }).wrap(\"<b/>\");") (text . "") (html . "<p>Hello <a href=\"http://ejohn.org/\">John</a>, how are you doing?</p>") (text . "")) ((text . "") (text . "Change the background colour of links inside of an iframe.

") (text . "") (js . "$(\"#frameDemo\").contents().find(\"a\").css(\"background-color\",\"#BADA55\");") (text . "") (html . "<iframe src=\"http://api.jquery.com/\" width=\"80%\" height=\"600\" id='frameDemo'></iframe> ") (text . ""))))) jquery-doc-hash)

(push "closest" jquery-methods)

(puthash "closest" (quote (("name" . "closest") ("signatures" "closest" (("selector" "A string containing a selector expression to match elements against.

" nil nil)) (("selector" "A string containing a selector expression to match elements against.

" nil nil) ("context" "A DOM element within which a matching element may be found. If no
context is passed in then the context of the jQuery set will be used
instead.
" "true" nil)) (("jQuery object" "A jQuery object to match elements against.

" nil nil)) (("element" "An element to match elements against.

" nil nil))) ("desc" (text . "Get the first ancestor element that matches the selector, beginning at
the current element and progressing up through the DOM tree.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.closest() method allows us to search through these elements and their
ancestors in the DOM tree and construct a new jQuery object from the
matching elements. The .parents() and .closest() methods are similar in
that they both traverse up the DOM tree. The differences between the
two, though subtle, are significant:
") (text . "") (text . ".closest() .parents()
Begins with the current element Begins with the parent element
Travels up the DOM tree until it finds a match for the supplied
selector Travels up the DOM tree to the documentʼs root element, adding
each ancestor element to a temporary collection; it then filters that
collection based on a selector if one is supplied
The returned jQuery object contains zero or one element The returned
jQuery object contains zero, one, or multiple elements
") (text . "") (js . "
		  <ul id=\"one\" class=\"level-1\">
		  <li class=\"item-i\">I</li>
		  <li id=\"ii\" class=\"item-ii\">II
		  <ul class=\"level-2\">
		  <li class=\"item-a\">A</li>
		  <li class=\"item-b\">B
		  <ul class=\"level-3\">
		  <li class=\"item-1\">1</li>
		  <li class=\"item-2\">2</li>
		  <li class=\"item-3\">3</li>
		  </ul>
		  </li>
		  <li class=\"item-c\">C</li>
		  </ul>
		  </li>
		  <li class=\"item-iii\">III</li>
		  </ul>
		") (text . "") (text . "Suppose we perform a search for <ul> elements starting at item A:


") (text . "") (js . "
		  $('li.item-a').closest('ul')
		  .css('background-color', 'red');
		") (text . "") (text . "This will change the color of the level-2 <ul>, since it is the first
encountered when traveling up the DOM tree.

") (text . "") (text . "Suppose we search for an <li> element instead:


") (text . "") (js . "$('li.item-a').closest('li')
		.css('background-color', 'red');
		") (text . "") (text . "This will change the color of list item A. The .closest() method begins
its search with the element itself before progressing up the DOM tree,
and stops when item A matches the selector.
") (text . "") (text . "We can pass in a DOM element as the context within which to search for
the closest element.

") (text . "") (js . "var listItemII = document.getElementById('ii');
		$('li.item-a').closest('ul', listItemII)
		.css('background-color', 'red');
		$('li.item-a').closest('#one', listItemII)
		.css('background-color', 'green');") (text . "") (text . "This will change the color of the level-2 <ul>, because it is both the
first <ul> ancestor of list item A and a descendant of list item II. It
will not change the color of the level-1 <ul>, however, because it is
not a descendant of list item II.
") (text . "")) ("examples" ((text . "") (text . "Show how event delegation can be done with closest. The closest list
element toggles a yellow background when it or its descendent is
clicked.
") (text . "") (js . "
		  $( document ).bind(\"click\", function( e ) {
		  $( e.target ).closest(\"li\").toggleClass(\"hilight\");
		  });
		  ") (text . "") (css . "
		  li { margin: 3px; padding: 3px; background: #EEEEEE; }
		  li.hilight { background: yellow; }
		  ") (text . "") (html . "<ul>
		  <li><b>Click me!</b></li>
		  <li>You can also <b>Click me!</b></li>
		  </ul>") (text . "")) ((text . "") (text . "Pass a jQuery object to closest. The closest list element toggles a
yellow background when it or its descendent is clicked.

") (text . "") (js . "
		  var $listElements = $(\"li\").css(\"color\", \"blue\");
		  $( document ).bind(\"click\", function( e ) {
		  $( e.target ).closest( $listElements ).toggleClass(\"hilight\");
		  });
		  ") (text . "") (css . "
		  li { margin: 3px; padding: 3px; background: #EEEEEE; }
		  li.hilight { background: yellow; }
		  ") (text . "") (html . "<ul>
		  <li><b>Click me!</b></li>
		  <li>You can also <b>Click me!</b></li>
		  </ul>") (text . ""))))) jquery-doc-hash)

(push "closest" jquery-methods)

(puthash "closest" (quote (("name" . "closest") ("signatures" "closest" (("selectors" "An array or string containing a selector expression to match elements
against (can also be a jQuery object).

" nil nil) ("context" "A DOM element within which a matching element may be found. If no
context is passed in then the context of the jQuery set will be used
instead.
" "true" nil))) ("desc" (text . "Gets an array of all the elements and selectors matched against the
current element up through the DOM tree.

")) ("longdesc" (text . "This method is primarily meant to be used internally or by plugin
authors.

")) ("examples" ((text . "") (text . "Show how event delegation can be done with closest.

") (text . "") (js . "
		  var close = $(\"li:first\").closest([\"ul\", \"body\"]);
		  $.each(close, function(i){
		  $(\"li\").eq(i).html( this.selector + \": \" + this.elem.nodeName );
		  });") (text . "") (css . "") (text . "") (html . "<ul><li></li><li></li></ul>") (text . ""))))) jquery-doc-hash)

(push "$.getJSON" jquery-methods)

(puthash "$.getJSON" (quote (("name" . "$.getJSON") ("signatures" "$.getJSON" (("url" "A string containing the URL to which the request is sent.

" nil nil) ("data" "A map or string that is sent to the server with the request.

" "true" nil) ("success(data, textStatus, jqXHR)" "A callback function that is executed if the request succeeds.

" "true" nil))) ("desc" (text . "Load JSON-encoded data from the server using a GET HTTP request.

")) ("longdesc" (text . "") (text . "This is a shorthand Ajax function, which is equivalent to:


") (text . "") (js . "$.ajax({
		  url: ") (text . "") (text . "Data that is sent to the server is appended to the URL as a query
string. If the value of the data parameter is an object (map), it is
converted to a string and url-encoded before it is appended to the URL.
") (text . "") (text . "Most implementations will specify a success handler:


") (text . "") (js . "$.getJSON('ajax/test.json', function(data) {
		  var items = [];

		  $.each(data, function(key, val) {
		  items.push('<li id=\"' + key + '\">' + val + '</li>');
		  });

		  $('<ul/>', {
		  'class': 'my-new-list',
		  html: items.join('')
		  }).appendTo('body');
		  });
		  ") (text . "") (text . "This example, of course, relies on the structure of the JSON file:


") (text . "") (js . "{
		  \"one\": \"Singular sensation\",
		  \"two\": \"Beady little eyes\",
		  \"three\": \"Little birds pitch by my doorstep\"
		  }
		  ") (text . "") (text . "Using this structure, the example loops through the requested data,
builds an unordered list, and appends it to the body.

") (text . "") (text . "The success callback is passed the returned data, which is typically a
JavaScript object or array as defined by the JSON structure and parsed
using the $.parseJSON() method. It is also passed the text status of
the response.
") (text . "") (text . "As of jQuery 1.5, the success callback function receives a \"jqXHR\"
object (in jQuery 1.4, it received the XMLHttpRequest object). However,
since JSONP and cross-domain GET requests do not use XHR, in those
cases the jqXHR and textStatus parameters passed to the success
callback are undefined.
") (text . "") (text . "  Important: As of jQuery 1.4, if the JSON file contains a syntax
  error, the request will usually fail silently. Avoid frequent
  hand-editing of JSON data for this reason. JSON is a
  data-interchange format with syntax rules that are stricter than
  those of JavaScriptʼs object literal notation. For example, all
  strings represented in JSON, whether they are properties or values,
  must be enclosed in double-quotes. For details on the JSON format,
  see http://json.org/.
") (text . "") (text . " JSONP


") (text . "") (text . "If the URL includes the string \"callback=?\" (or similar, as defined by
the server-side API), the request is treated as JSONP instead. See the
discussion of the jsonp data type in $.ajax() for more details.
") (text . "") (text . " The jqXHR Object


") (text . "") (text . "As of jQuery 1.5, all of jQueryʼs Ajax methods return a superset of the
XMLHTTPRequest object. This jQuery XHR object, or \"jqXHR,\" returned by
$.getJSON() implements the Promise interface, giving it all the
properties, methods, and behavior of a Promise (see Deferred object for
more information). For convenience and consistency with the callback
names used by $.ajax() , it provides .error(), .success(), and
.complete() methods. These methods take a function argument that is
called when the request terminates, and the function receives the same
arguments as the correspondingly-named $.ajax() callback.
") (text . "") (text . "The Promise interface in jQuery 1.5 also allows jQueryʼs Ajax methods,
including $.getJSON(), to chain multiple .success(), .complete(), and
.error() callbacks on a single request, and even to assign these
callbacks after the request may have completed. If the request is
already complete, the callback is fired immediately.
") (text . "") (js . "// Assign handlers immediately after making the request,
		  // and remember the jqxhr object for this request
		  var jqxhr = $.getJSON(\"example.json\", function() {
		  alert(\"success\");
		  })
		  .success(function() { alert(\"second success\"); })
		  .error(function() { alert(\"error\"); })
		  .complete(function() { alert(\"complete\"); });

		  // perform other work here ...

		  // Set another completion function for the request above
		  jqxhr.complete(function(){ alert(\"second complete\"); });") (text . "")) ("examples" ((text . "") (text . "Loads the four most recent cat pictures from the Flickr JSONP API.

") (text . "") (js . "
		  $.getJSON(\"http://api.flickr.com/services/feeds/photos_public.gne?jsoncallback=?\",
		  {
		  tags: \"cat\",
		  tagmode: \"any\",
		  format: \"json\"
		  },
		  function(data) {
		  $.each(data.items, function(i,item){
		  $(\"<img/>\").attr(\"src\", item.media.m).appendTo(\"#images\");
		  if ( i == 3 ) return false;
		  });
		  });") (text . "") (html . "<div id=\"images\">

		  </div>") (text . "") (css . "img{ height: 100px; float: left; }") (text . "")) ((text . "") (text . "Load the JSON data from test.js and access a name from the returned
JSON data.

") (text . "") (js . "$.getJSON(\"test.js\", function(json) {
		  alert(\"JSON Data: \" + json.users[3].name);
		  });") (text . "")) ((text . "") (text . "Load the JSON data from test.js, passing along additional data, and
access a name from the returned JSON data.

") (text . "") (js . "$.getJSON(\"test.js\", { name: \"John\", time: \"2pm\" }, function(json) {
		  alert(\"JSON Data: \" + json.users[3].name);
		  });") (text . ""))))) jquery-doc-hash)

(push "$.get" jquery-methods)

(puthash "$.get" (quote (("name" . "$.get") ("signatures" "$.get" (("url" "A string containing the URL to which the request is sent.

" nil nil) ("data" "A map or string that is sent to the server with the request.

" "true" nil) ("success(data, textStatus, jqXHR)" "A callback function that is executed if the request succeeds.

" "true" nil) ("dataType" "The type of data expected from the server. Default: Intelligent Guess
(xml, json, script, or html).

" "true" nil))) ("desc" (text . "Load data from the server using a HTTP GET request.

")) ("longdesc" (text . "This is a shorthand Ajax function, which is equivalent to:


") (text . "") (js . "$.ajax({
		  url: ") (text . "") (text . "The success callback function is passed the returned data, which will
be an XML root element, text string, JavaScript file, or JSON object,
depending on the MIME type of the response. It is also passed the text
status of the response.
") (text . "") (text . "As of jQuery 1.5, the success callback function is also passed a
\"jqXHR\" object (in jQuery 1.4, it was passed the XMLHttpRequest
object). However, since JSONP and cross-domain GET requests do not use
XHR, in those cases the (j)XHR and textStatus parameters passed to the
success callback are undefined.
") (text . "") (text . "Most implementations will specify a success handler:


") (text . "") (js . "$.get('ajax/test.html', function(data) {
		  $('.result').html(data);
		  alert('Load was performed.');
		  });
		  ") (text . "") (text . "This example fetches the requested HTML snippet and inserts it on the
page.

") (text . "") (text . " The jqXHR Object


") (text . "") (text . "As of jQuery 1.5, all of jQueryʼs Ajax methods return a superset of the
XMLHTTPRequest object. This jQuery XHR object, or \"jqXHR,\" returned by
$.get() implements the Promise interface, giving it all the properties,
methods, and behavior of a Promise (see Deferred object for more
information). For convenience and consistency with the callback names
used by $.ajax() , it provides .error(), .success(), and .complete()
methods. These methods take a function argument that is called when the
request terminates, and the function receives the same arguments as the
correspondingly-named $.ajax() callback.
") (text . "") (text . "The Promise interface in jQuery 1.5 also allows jQueryʼs Ajax methods,
including $.get(), to chain multiple .success(), .complete(), and
.error() callbacks on a single request, and even to assign these
callbacks after the request may have completed. If the request is
already complete, the callback is fired immediately.
") (text . "") (js . "// Assign handlers immediately after making the request,
		  // and remember the jqxhr object for this request
		  var jqxhr = $.get(\"example.php\", function() {
		  alert(\"success\");
		  })
		  .success(function() { alert(\"second success\"); })
		  .error(function() { alert(\"error\"); })
		  .complete(function() { alert(\"complete\"); });

		  // perform other work here ...

		  // Set another completion function for the request above
		  jqxhr.complete(function(){ alert(\"second complete\"); });") (text . "")) ("examples" ((text . "") (text . "Request the test.php page, but ignore the return results.

") (text . "") (js . "$.get(\"test.php\");") (text . "")) ((text . "") (text . "Request the test.php page and send some additional data along (while
still ignoring the return results).

") (text . "") (js . "$.get(\"test.php\", { name: \"John\", time: \"2pm\" } );") (text . "")) ((text . "") (text . "pass arrays of data to the server (while still ignoring the return
results).

") (text . "") (js . "$.get(\"test.php\", { 'choices[]': [\"Jon\", \"Susan\"]} );") (text . "")) ((text . "") (text . "Alert out the results from requesting test.php (HTML or XML, depending
on what was returned).

") (text . "") (js . "$.get(\"test.php\", function(data){
		    alert(\"Data Loaded: \" + data);
		    });") (text . "")) ((text . "") (text . "Alert out the results from requesting test.cgi with an additional
payload of data (HTML or XML, depending on what was returned).

") (text . "") (js . "$.get(\"test.cgi\", { name: \"John\", time: \"2pm\" },
		    function(data){
		    alert(\"Data Loaded: \" + data);
		    });") (text . "")) ((text . "") (text . "Gets the test.php page contents which has been returned in json format
(<?php echo json_encode(array(\"name\"=>\"John\",\"time\"=>\"2pm\")); ?>).

") (text . "") (js . "$.get(\"test.php\", { \"func\": \"getNameAndTime\" },
		    function(data){
		    alert(data.name); // John
		    console.log(data.time); //  2pm
		    }, \"json\");") (text . ""))))) jquery-doc-hash)

(push "load" jquery-methods)

(puthash "load" (quote (("name" . "load") ("signatures" "load" (("url" "A string containing the URL to which the request is sent.

" nil nil) ("data" "A map or string that is sent to the server with the request.

" "true " nil) ("complete(responseText, textStatus, XMLHttpRequest)" "A callback function that is executed when the request completes.

" "true" nil))) ("desc" (text . "Load data from the server and place the returned HTML into the matched
element.

")) ("longdesc" (text . "") (text . "This method is the simplest way to fetch data from the server. It is
roughly equivalent to $.get(url, data, success) except that it is a
method rather than global function and it has an implicit callback
function. When a successful response is detected (i.e. when textStatus
is \"success\" or \"notmodified\"), .load() sets the HTML contents of the
matched element to the returned data. This means that most uses of the
method can be quite simple:
") (text . "") (js . "$('#result').load('ajax/test.html');") (text . "") (text . "The provided callback, if any, is executed after this post-processing
has been performed:

") (text . "") (js . "$('#result').load('ajax/test.html', function() {
		      alert('Load was performed.');
		      });") (text . "") (text . "In the two examples above, if the current document does not contain an
element with an ID of \"result,\" the .load() method is not executed.

") (text . "") (text . "The POST method is used if data is provided as an object; otherwise,
GET is assumed.

") (text . "") (text . "  Note: The event handling suite also has a method named .load() .
  Which one is fired depends on the set of arguments passed.

") (text . "") (text . " Loading Page Fragments


") (text . "") (text . "The .load() method, unlike $.get() , allows us to specify a portion of
the remote document to be inserted. This is achieved with a special
syntax for the url parameter. If one or more space characters are
included in the string, the portion of the string following the first
space is assumed to be a jQuery selector that determines the content to
be loaded.
") (text . "") (text . "We could modify the example above to use only part of the document that
is fetched:

") (text . "") (js . "$('#result').load('ajax/test.html #container');") (text . "") (text . "When this method executes, it retrieves the content of ajax/test.html,
but then jQuery parses the returned document to find the element with
an ID of container. This element, along with its contents, is inserted
into the element with an ID of result, and the rest of the retrieved
document is discarded.
") (text . "") (text . "jQuery uses the browserʼs .innerHTML property to parse the retrieved
document and insert it into the current document. During this process,
browsers often filter elements from the document such as <html>,
<title>, or <head> elements. As a result, the elements retrieved by
.load() may not be exactly the same as if the document were retrieved
directly by the browser.
") (text . "")) ("examples" ((text . "") (text . "Load the main pageʼs footer navigation into an ordered list.

") (text . "") (js . "
		      $(\"#new-nav\").load(\"/ #jq-footerNavigation li\");
		      ") (text . "") (css . "
		      body{ font-size: 12px; font-family: Arial; }
		      ") (text . "") (html . "
		      <b>Footer navigation:</b>
		      <ol id=\"new-nav\"></ol>
		      ") (text . "")) ((text . "") (text . "Display a notice if the Ajax request encounters an error.

") (text . "") (js . "
		      $(\"#success\").load(\"/not-here.php\", function(response, status, xhr) {
		      if (status == \"error\") {
		      var msg = \"Sorry but there was an error: \";
		      $(\"#error\").html(msg + xhr.status + \" \" + xhr.statusText);
		      }
		      });
		      ") (text . "") (css . "
		      body{ font-size: 12px; font-family: Arial; }
		      ") (text . "") (html . "
		      <b>Successful Response (should be blank):</b>
		      <div id=\"success\"></div>
		      <b>Error Response:</b>
		      <div id=\"error\"></div>
		      ") (text . "")) ((text . "") (text . "Load the feeds.html file into the div with the ID of feeds.

") (text . "") (js . "$(\"#feeds\").load(\"feeds.html\");") (text . "") (text . "<div id=\"feeds\"><b>45</b> feeds found.</div>

") (text . "")) ((text . "") (text . "pass arrays of data to the server.

") (text . "") (js . "$(\"#objectID\").load(\"test.php\", { 'choices[]': [\"Jon\", \"Susan\"] } );") (text . "")) ((text . "") (text . "Same as above, but will POST the additional parameters to the server
and a callback that is executed when the server is finished responding.

") (text . "") (js . "$(\"#feeds\").load(\"feeds.php\", {limit: 25}, function(){
		      alert(\"The last 25 entries in the feed have been loaded\");
		      });") (text . ""))))) jquery-doc-hash)

(push "$.ajax" jquery-methods)

(puthash "$.ajax" (quote (("name" . "$.ajax") ("signatures" "$.ajax" (("url" "A string containing the URL to which the request is sent.

" nil nil) ("settings" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().
See jQuery.ajax( settings ) below for a complete list of all settings.
" "true" nil)) (("settings" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

" nil (("accepts" "Map" "depends on DataType" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("async" "Boolean" "true" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("beforeSend(jqXHR, settings)" "Function" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("cache" "Boolean" "true, false for dataType 'script' and 'jsonp'" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("complete(jqXHR, textStatus)" "Function, Array" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("contents" "Map" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("contentType" "String" "'application/x-www-form-urlencoded'" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("context" "Object" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("converters" "Map" "{\"* text\": window.String, \"text html\": true, \"text json\": jQuery.parseJSON, \"text xml\": jQuery.parseXML}" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("crossDomain" "" "false for same-domain requests, true for cross-domain requests" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("data" "Object, String" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("dataFilter(data, type)" "Function" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("dataType" "String" "Intelligent Guess (xml, json, script, or html)" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("error(jqXHR, textStatus, errorThrown)" "Function" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("global" "Boolean" "true" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("headers" "Map" "{}" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("ifModified" "Boolean" "false" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("isLocal" "Boolean" "depends on current location protocol" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("jsonp" "String" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("jsonpCallback" "String, Function" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("mimeType" "String" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("password" "String" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("processData" "Boolean" "true" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("scriptCharset" "String" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("statusCode" "Map" "{}" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("success(data, textStatus, jqXHR)" "Function, Array" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("timeout" "Number" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("traditional" "Boolean" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("type" "String" "'GET'" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("url" "String" "The current page" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("username" "String" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("xhr" "Function" "ActiveXObject when available (IE), the XMLHttpRequest otherwise" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

") ("xhrFields" "Map" "" "A set of key/value pairs that configure the Ajax request. All settings
are optional. A default can be set for any option with $.ajaxSetup().

"))))) ("desc" (text . "Perform an asynchronous HTTP (Ajax) request.

")) ("longdesc" (text . "") (text . "The $.ajax() function underlies all Ajax requests sent by jQuery. It is
often unnecessary to directly call this function, as several
higher-level alternatives like $.get() and .load() are available and
are easier to use. If less common options are required, though,
$.ajax() can be used more flexibly.
") (text . "") (text . "At its simplest, the $.ajax() function can be called with no arguments:


") (text . "") (js . "$.ajax();") (text . "") (text . "Note: Default settings can be set globally by using the $.ajaxSetup()
function.

") (text . "") (text . "This example, using no options, loads the contents of the current page,
but does nothing with the result. To use the result, we can implement
one of the callback functions.
") (text . "") (text . " The jqXHR Object


") (text . "") (text . "The jQuery XMLHttpRequest (jqXHR) object returned by $.ajax() as of
jQuery 1.5 is a superset of the browserʼs native XMLHttpRequest object.
For example, it contains responseText and responseXML properties, as
well as a getResponseHeader() method. When the transport mechanism is
something other than XMLHttpRequest (for example, a script tag for a
JSONP request) the jqXHR object simulates native XHR functionality
where possible.
") (text . "") (text . "As of jQuery 1.5.1, the jqXHR object also contains the overrideMimeType
method.

") (text . "") (text . "The jqXHR objects returned by $.ajax() implement the Promise interface,
giving them all the properties, methods, and behavior of a Promise (see
Deferred object for more information). For convenience and consistency
with the callback names used by $.ajax(), jqXHR also provides .error(),
.success(), and .complete() methods. These methods take a function
argument that is called when the $.ajax() request terminates, and the
function receives the same arguments as the correspondingly-named
$.ajax() callback. In jQuery 1.5 this allows you to assign multiple
callbacks on a single request, and even to assign callbacks after the
request may have completed. (If the request is already complete, the
callback is fired immediately.)
") (text . "") (js . "// Assign handlers immediately after making the request,
			// and remember the jqxhr object for this request
			var jqxhr = $.ajax({ url: \"example.php\" })
			.success(function() { alert(\"success\"); })
			.error(function() { alert(\"error\"); })
			.complete(function() { alert(\"complete\"); });
			
			// perform other work here ...
			
			// Set another completion function for the request above
			jqxhr.complete(function(){ alert(\"second complete\"); });") (text . "") (text . "For backward compatibility with XMLHttpRequest, a jqXHR object will
expose the following properties and methods:

") (text . "") (text . "  * readyState
  * status
  * statusText
  * responseXML and/or responseText when the underlying request
    responded with xml and/or text, respectively
  * setRequestHeader(name, value) which departs from the standard by
    replacing the old value with the new one rather than concatenating
    the new value to the old one
  * getAllResponseHeaders()
  * getResponseHeader()
  * abort()
") (text . "") (text . "No onreadystatechange mechanism is provided, however, since success,
error, complete and statusCode cover all conceivable requirements.

") (text . "") (text . " Callback Function Queues


") (text . "") (text . "The beforeSend, error, dataFilter, success and complete options all
accept callback functions that are invoked at the appropriate times.

") (text . "") (text . "As of jQuery 1.5, the error, success, and complete callback hooks are
first-in, first-out managed queues. This means you can assign more than
one callback for each hook. See Deferred object methods, which are
implemented internally for these $.ajax() callback hooks.
") (text . "") (text . "The this reference within all callbacks is the object in the context
option passed to $.ajax in the settings; if context is not specified,
this is a reference to the Ajax settings themselves.
") (text . "") (text . "Some types of Ajax requests, such as JSONP and cross-domain GET
requests, do not use XHR; in those cases the XMLHttpRequest and
textStatus parameters passed to the callback are undefined.
") (text . "") (text . "Here are the callback hooks provided by $.ajax():


") (text . "") (text . " 1. beforeSend callback is invoked; it receives the jqXHR object and
    the settings map as parameters.
 2. error callbacks are invoked, in the order they are registered, if
    the request fails. They receive the jqXHR, a string indicating the
    error type, and an exception object if applicable. Some built-in
    errors will provide a string as the exception object: \"abort\",
    \"timeout\", \"No Transport\".
 3. dataFilter callback is invoked immediately upon successful receipt
    of response data. It receives the returned data and the value of
    dataType, and must return the (possibly altered) data to pass on to
    success.
 4. success callbacks are then invoked, in the order they are
    registered, if the request succeeds. They receive the returned
    data, a string containing the success code, and the jqXHR object.
 5. complete callbacks fire, in the order they are registered, when the
    request finishes, whether in failure or success. They receive the
    jqXHR object, as well as a string containing the success or error
    code.
") (text . "") (text . "For example, to make use of the returned HTML, we can implement a
success handler:

") (text . "") (js . "$.ajax({
			url: 'ajax/test.html',
			success: function(data) {
			$('.result').html(data);
			alert('Load was performed.');
			}
			});") (text . "") (text . " Data Types


") (text . "") (text . "The $.ajax() function relies on the server to provide information about
the retrieved data. If the server reports the return data as XML, the
result can be traversed using normal XML methods or jQueryʼs selectors.
If another type is detected, such as HTML in the example above, the
data is treated as text.
") (text . "") (text . "Different data handling can be achieved by using the dataType option.
Besides plain xml, the dataType can be html, json, jsonp, script, or
text.
") (text . "") (text . "The text and xml types return the data with no processing. The data is
simply passed on to the success handler, either through the
responseText or responseXML property of the jqXHR object, respectively.
") (text . "") (text . "Note: We must ensure that the MIME type reported by the web server
matches our choice of dataType. In particular, XML must be declared by
the server as text/xml or application/xml for consistent results.
") (text . "") (text . "If html is specified, any embedded JavaScript inside the retrieved data
is executed before the HTML is returned as a string. Similarly, script
will execute the JavaScript that is pulled back from the server, then
return nothing.
") (text . "") (text . "The json type parses the fetched data file as a JavaScript object and
returns the constructed object as the result data. To do so, it uses
jQuery.parseJSON() when the browser supports it; otherwise it uses a
Function constructor. Malformed JSON data will throw a parse error (see
json.org for more information). JSON data is convenient for
communicating structured data in a way that is concise and easy for
JavaScript to parse. If the fetched data file exists on a remote
server, specify the jsonp type instead.
") (text . "") (text . "The jsonp type appends a query string parameter of callback=? to the
URL. The server should prepend the JSON data with the callback name to
form a valid JSONP response. We can specify a parameter name other than
callback with the jsonp option to $.ajax().
") (text . "") (text . "Note: JSONP is an extension of the JSON format, requiring some
server-side code to detect and handle the query string parameter. More
information about it can be found in the original post detailing its
use.
") (text . "") (text . "When data is retrieved from remote servers (which is only possible
using the script or jsonp data types), the error callbacks and global
events will never be fired.
") (text . "") (text . " Sending Data to the Server


") (text . "") (text . "By default, Ajax requests are sent using the GET HTTP method. If the
POST method is required, the method can be specified by setting a value
for the type option. This option affects how the contents of the data
option are sent to the server. POST data will always be transmitted to
the server using UTF-8 charset, per the W3C XMLHTTPRequest standard.
") (text . "") (text . "The data option can contain either a query string of the form
key1=value1&key2=value2, or a map of the form {key1: ʼvalue1ʼ, key2:
ʼvalue2ʼ}. If the latter form is used, the data is converted into a
query string using jQuery.param() before it is sent. This processing
can be circumvented by setting processData to false. The processing
might be undesirable if you wish to send an XML object to the server;
in this case, change the contentType option from
application/x-www-form-urlencoded to a more appropriate MIME type.
") (text . "") (text . " Advanced Options


") (text . "") (text . "The global option prevents handlers registered using .ajaxSend() ,
.ajaxError() , and similar methods from firing when this request would
trigger them. This can be useful to, for example, suppress a loading
indicator that was implemented with .ajaxSend() if the requests are
frequent and brief. With cross-domain script and JSONP requests, the
global option is automatically set to false. See the descriptions of
these methods below for more details. See the descriptions of these
methods below for more details.
") (text . "") (text . "If the server performs HTTP authentication before providing a response,
the user name and password pair can be sent via the username and
password options.
") (text . "") (text . "Ajax requests are time-limited, so errors can be caught and handled to
provide a better user experience. Request timeouts are usually either
left at their default or set as a global default using $.ajaxSetup()
rather than being overridden for specific requests with the timeout
option.
") (text . "") (text . "By default, requests are always issued, but the browser may serve
results out of its cache. To disallow use of the cached results, set
cache to false. To cause the request to report failure if the asset has
not been modified since the last request, set ifModified to true.
") (text . "") (text . "The scriptCharset allows the character set to be explicitly specified
for requests that use a <script> tag (that is, a type of script or
jsonp). This is useful if the script and host page have differing
character sets.
") (text . "") (text . "The first letter in Ajax stands for \"asynchronous,\" meaning that the
operation occurs in parallel and the order of completion is not
guaranteed. The async option to $.ajax() defaults to true, indicating
that code execution can continue after the request is made. Setting
this option to false (and thus making the call no longer asynchronous)
is strongly discouraged, as it can cause the browser to become
unresponsive.
") (text . "") (text . "The $.ajax() function returns the XMLHttpRequest object that it
creates. Normally jQuery handles the creation of this object
internally, but a custom function for manufacturing one can be
specified using the xhr option. The returned object can generally be
discarded, but does provide a lower-level interface for observing and
manipulating the request. In particular, calling .abort() on the object
will halt the request before it completes.
") (text . "") (text . " Extending Ajax


") (text . "") (text . "As of jQuery 1.5, jQueryʼs Ajax implementation includes prefilters,
converters, and transports that allow you to extend Ajax with a great
deal of flexibility. For more information about these advanced
features, see the Extending Ajax page.
") (text . "")) ("examples" ((text . "") (text . "Load and execute a JavaScript file.

") (text . "") (js . "$.ajax({
			type: \"GET\",
			url: \"test.js\",
			dataType: \"script\"
			});") (text . "")) ((text . "") (text . "Save some data to the server and notify the user once itʼs complete.

") (text . "") (js . "$.ajax({
			type: \"POST\",
			url: \"some.php\",
			data: \"name=John&location=Boston\",
			success: function(msg){
			alert( \"Data Saved: \" + msg );
			}
			});") (text . "")) ((text . "") (text . "Retrieve the latest version of an HTML page.

") (text . "") (js . "$.ajax({
			url: \"test.html\",
			cache: false,
			success: function(html){
			$(\"#results\").append(html);
			}
			});") (text . "")) ((text . "") (text . "Loads data synchronously. Blocks the browser while the requests is
active. It is better to block user interaction by other means when
synchronization is necessary.
") (text . "") (js . "var html = $.ajax({
			url: \"some.php\",
			async: false
			}).responseText;") (text . "")) ((text . "") (text . "Sends an xml document as data to the server. By setting the processData
option to false, the automatic conversion of data to strings is
prevented.
") (text . "") (js . "var xmlDocument = [create xml document];
			$.ajax({
			url: \"page.php\",
			processData: false,
			data: xmlDocument,
			success: handleResponse
			});") (text . "")) ((text . "") (text . "Sends an id as data to the server, save some data to the server and
notify the user once itʼs complete. Note that this usage - returning
the result of the call into a variable - requires a synchronous
(blocking) request! (async:false)
") (text . "") (js . "bodyContent = $.ajax({
			url: \"script.php\",
			global: false,
			type: \"POST\",
			data: ({id : this.getAttribute('id')}),
			dataType: \"html\",
			async:false,
			success: function(msg){
			alert(msg);
			}
			}
			).responseText;") (text . ""))))) jquery-doc-hash)

(push "children" jquery-methods)

(puthash "children" (quote (("name" . "children") ("signatures" "children" (("selector" "A string containing a selector expression to match elements against.

" "true" nil))) ("desc" (text . "Get the children of each element in the set of matched elements,
optionally filtered by a selector.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.children() method allows us to search through the immediate children
of these elements in the DOM tree and construct a new jQuery object
from the matching elements. The .find() and .children() methods are
similar, except that the latter only travels a single level down the
DOM tree. Note also that like most jQuery methods, .children() does not
return text nodes; to get all children including text and comment
nodes, use .contents().
") (text . "") (text . "The method optionally accepts a selector expression of the same type
that we can pass to the $() function. If the selector is supplied, the
elements will be filtered by testing whether they match it.
") (text . "") (text . "Consider a page with a basic nested list on it:


") (text . "") (js . "
			  <ul class=\"level-1\">
			  <li class=\"item-i\">I</li>
			  <li class=\"item-ii\">II
			  <ul class=\"level-2\">
			  <li class=\"item-a\">A</li>
			  <li class=\"item-b\">B
			  <ul class=\"level-3\">
			  <li class=\"item-1\">1</li>
			  <li class=\"item-2\">2</li>
			  <li class=\"item-3\">3</li>
			  </ul>
			  </li>
			  <li class=\"item-c\">C</li>
			  </ul>
			  </li>
			  <li class=\"item-iii\">III</li>
			  </ul>
			") (text . "") (text . "If we begin at the level-2 list, we can find its children:


") (text . "") (js . "$('ul.level-2').children().css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind items A, B, and C.
Since we do not supply a selector expression, all of the children are
part of the returned jQuery object. If we had supplied one, only the
matching items among these three would be included.
")) ("examples" ((text . "") (text . "Find all children of the clicked element.

") (text . "") (js . "
			  
			  $(\"#container\").click(function (e) {
			  $(\"*\").removeClass(\"hilite\");
			  var $kids = $(e.target).children();
			  var len = $kids.addClass(\"hilite\").length;
			  
			  $(\"#results span:first\").text(len);
			  $(\"#results span:last\").text(e.target.tagName);
			  
			  e.preventDefault();
			  return false;
			  });
			  ") (text . "") (css . "
			  body { font-size:16px; font-weight:bolder; }
			  div { width:130px; height:82px; margin:10px; float:left;
			  border:1px solid blue; padding:4px; }
			  #container { width:auto; height:105px; margin:0; float:none;
			  border:none; }
			  .hilite { border-color:red; }
			  #results { display:block; color:red; }
			  p { margin:10px; border:1px solid transparent; }
			  span { color:blue; border:1px solid transparent; }
			  input { width:100px; }
			  em { border:1px solid transparent; }
			  a { border:1px solid transparent; }
			  b { border:1px solid transparent; }
			  button { border:1px solid transparent; }
			  ") (text . "") (html . "<div id=\"container\">
			  
			  <div>
			  <p>This <span>is the <em>way</em> we</span> 
			  write <em>the</em> demo,</p>
			  
			  </div>
			  <div>
			  <a href=\"#\"><b>w</b>rit<b>e</b></a> the <span>demo,</span> <button>write 
			  the</button> demo,
			  </div>
			  
			  <div>
			  This <span>the way we <em>write</em> the <em>demo</em> so</span>
			  
			  <input type=\"text\" value=\"early\" /> in
			  </div>
			  <p>
			  <span>t</span>he <span>m</span>orning.
			  <span id=\"results\">Found <span>0</span> children in <span>TAG</span>.</span>
			  
			  </p>
			  </div>") (text . "")) ((text . "") (text . "Find all children of each div.

") (text . "") (js . "$(\"div\").children().css(\"border-bottom\", \"3px double red\");") (text . "") (css . "
			  body { font-size:16px; font-weight:bolder; }
			  span { color:blue; }
			  p { margin:5px 0; }
			  ") (text . "") (html . "<p>Hello (this is a paragraph)</p>
			  
			  <div><span>Hello Again (this span is a child of the a div)</span></div>
			  <p>And <span>Again</span> (in another paragraph)</p>
			  
			  <div>And One Last <span>Time</span> (most text directly in a div)</div>") (text . "")) ((text . "") (text . "Find all children with a class \"selected\" of each div.

") (text . "") (js . "$(\"div\").children(\".selected\").css(\"color\", \"blue\");") (text . "") (css . "
			  
			  body { font-size:16px; font-weight:bolder; }
			  p { margin:5px 0; }
			  ") (text . "") (html . "<div>
			  <span>Hello</span>
			  <p class=\"selected\">Hello Again</p>
			  <div class=\"selected\">And Again</div>
			  
			  <p>And One Last Time</p>
			  </div>") (text . ""))))) jquery-doc-hash)

(push "add" jquery-methods)

(puthash "add" (quote (("name" . "add") ("signatures" "add" (("selector" "A string representing a selector expression to find additional elements
to add to the set of matched elements.

" nil nil)) (("elements" "One or more elements to add to the set of matched elements.

" nil nil)) (("html" "An HTML fragment to add to the set of matched elements.

" nil nil)) (("selector" "A string representing a selector expression to find additional elements
to add to the set of matched elements.

" nil nil) ("context" "The point in the document at which the selector should begin matching;
similar to the context argument of the $(selector, context) method.

" nil nil))) ("desc" (text . "Add elements to the set of matched elements.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the .add()
method constructs a new jQuery object from the union of those elements
and the ones passed into the method. The argument to .add() can be
pretty much anything that $() accepts, including a jQuery selector
expression, references to DOM elements, or an HTML snippet.
") (text . "") (text . "The updated set of elements can be used in a following (chained)
method, or assigned to a variable for later use. For example:

") (text . "") (js . "
			  $(\"p\").add(\"div\").addClass(\"widget\");
			  var pdiv = $(\"p\").add(\"div\");
			") (text . "") (text . "The following will not save the added elements, because the .add()
method creates a new set and leaves the original set in pdiv unchanged:

") (text . "") (js . "
			  var pdiv = $(\"p\");
			  pdiv.add(\"div\");  // WRONG, pdiv will not change
			") (text . "") (text . "Consider a page with a simple list and a paragraph following it:


") (text . "") (js . "<ul>
			<li>list item 1</li>
			<li>list item 2</li>
			<li>list item 3</li>
			</ul>
			<p>a paragraph</p>") (text . "") (text . "We can select the list items and then the paragraph by using either a
selector or a reference to the DOM element itself as the .add()
methodʼs argument:
") (text . "") (js . "$('li').add('p').css('background-color', 'red');") (text . "") (text . "Or:


") (text . "") (js . "$('li').add(document.getElementsByTagName('p')[0])
			.css('background-color', 'red');") (text . "") (text . "The result of this call is a red background behind all four elements.
Using an HTML snippet as the .add() methodʼs argument (as in the third
version), we can create additional elements on the fly and add those
elements to the matched set of elements. Letʼs say, for example, that
we want to alter the background of the list items along with a newly
created paragraph:
") (text . "") (js . "$('li').add('<p id=\"new\">new paragraph</p>')
			.css('background-color', 'red');") (text . "") (text . "Although the new paragraph has been created and its background color
changed, it still does not appear on the page. To place it on the page,
we could add one of the insertion methods to the chain.
") (text . "") (text . "As of jQuery 1.4 the results from .add() will always be returned in
document order (rather than a simple concatenation).

") (text . "")) ("examples" ((text . "") (text . "Finds all divs and makes a border. Then adds all paragraphs to the
jQuery object to set their backgrounds yellow.

") (text . "") (js . "
			  
			  $(\"div\").css(\"border\", \"2px solid red\")
			  .add(\"p\")
			  .css(\"background\", \"yellow\");
			  ") (text . "") (css . "
			  div { width:60px; height:60px; margin:10px; float:left; }
			  p { clear:left; font-weight:bold; font-size:16px; 
			  color:blue; margin:0 10px; padding:2px; }
			  ") (text . "") (html . "<div></div>
			  
			  <div></div>
			  <div></div>
			  <div></div>
			  <div></div>
			  <div></div>
			  
			  <p>Added this... (notice no border)</p>") (text . "")) ((text . "") (text . "Adds more elements, matched by the given expression, to the set of
matched elements.

") (text . "") (js . "$(\"p\").add(\"span\").css(\"background\", \"yellow\");") (text . "") (html . "<p>Hello</p><span>Hello Again</span>") (text . "")) ((text . "") (text . "Adds more elements, created on the fly, to the set of matched elements.


") (text . "") (js . "$(\"p\").clone().add(\"<span>Again</span>\").appendTo(document.body);") (text . "") (html . "<p>Hello</p>") (text . "")) ((text . "") (text . "Adds one or more Elements to the set of matched elements.

") (text . "") (js . "$(\"p\").add(document.getElementById(\"a\")).css(\"background\", \"yellow\");") (text . "") (html . "<p>Hello</p><span id=\"a\">Hello Again</span>") (text . "")) ((text . "") (text . "Demonstrates how to add (or push) elements to an existing collection

") (text . "") (js . "var collection = $(\"p\");
			  // capture the new collection
			  collection = collection.add(document.getElementById(\"a\"));
			  collection.css(\"background\", \"yellow\");") (text . "") (html . "<p>Hello</p><span id=\"a\">Hello Again</span>") (text . ""))))) jquery-doc-hash)

(push "not" jquery-methods)

(puthash "not" (quote (("name" . "not") ("signatures" "not" (("selector" "A string containing a selector expression to match elements against.

" nil nil)) (("elements" "One or more DOM elements to remove from the matched set.

" nil nil)) (("function(index)" "A function used as a test for each element in the set. this is the
current DOM element.

" nil nil))) ("desc" (text . "Remove elements from the set of matched elements.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the .not()
method constructs a new jQuery object from a subset of the matching
elements. The supplied selector is tested against each element; the
elements that donʼt match the selector will be included in the result.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
			  <ul>
			  <li>list item 1</li>
			  <li>list item 2</li>
			  <li>list item 3</li>
			  <li>list item 4</li>
			  <li>list item 5</li>
			  </ul>
			") (text . "") (text . "We can apply this method to the set of list items:


") (text . "") (js . "$('li').not(':even').css('background-color', 'red');") (text . "") (text . "The result of this call is a red background for items 2 and 4, as they
do not match the selector (recall that :even and :odd use 0-based
indexing).
") (text . "") (text . " Removing Specific Elements


") (text . "") (text . "The second version of the .not() method allows us to remove elements
from the matched set, assuming we have found those elements previously
by some other means. For example, suppose our list had an id applied to
one of its items:
") (text . "") (js . "
			  <ul>
			  <li>list item 1</li>
			  <li>list item 2</li>
			  <li id=\"notli\">list item 3</li>
			  <li>list item 4</li>
			  <li>list item 5</li>
			  </ul>
			") (text . "") (text . "We can fetch the third list item using the native JavaScript
getElementById() function, then remove it from a jQuery object:

") (text . "") (js . "
			  $('li').not(document.getElementById('notli'))
			  .css('background-color', 'red');
			") (text . "") (text . "This statement changes the color of items 1, 2, 4, and 5. We could have
accomplished the same thing with a simpler jQuery expression, but this
technique can be useful when, for example, other libraries provide
references to plain DOM nodes.
") (text . "") (text . "As of jQuery 1.4, the .not() method can take a function as its argument
in the same way that .filter() does. Elements for which the function
returns true are excluded from the filtered set; all other elements are
included.
")) ("examples" ((text . "") (text . "Adds a border to divs that are not green or blue.

") (text . "") (js . "
			  $(\"div\").not(\".green, #blueone\")
			  .css(\"border-color\", \"red\");
			  
			  ") (text . "") (css . "
			  div { width:50px; height:50px; margin:10px; float:left;
			  background:yellow; border:2px solid white; }
			  .green { background:#8f8; }
			  .gray { background:#ccc; }
			  #blueone { background:#99f; }
			  ") (text . "") (html . "<div></div>
			  <div id=\"blueone\"></div>
			  <div></div>
			  <div class=\"green\"></div>
			  
			  <div class=\"green\"></div>
			  <div class=\"gray\"></div>
			  <div></div>") (text . "")) ((text . "") (text . "Removes the element with the ID \"selected\" from the set of all
paragraphs.

") (text . "") (js . "$(\"p\").not( $(\"#selected\")[0] )") (text . "")) ((text . "") (text . "Removes the element with the ID \"selected\" from the set of all
paragraphs.

") (text . "") (js . "$(\"p\").not(\"#selected\")") (text . "")) ((text . "") (text . "Removes all elements that match \"div p.selected\" from the total set of
all paragraphs.

") (text . "") (js . "$(\"p\").not($(\"div p.selected\"))") (text . ""))))) jquery-doc-hash)

(push "outerWidth" jquery-methods)

(puthash "outerWidth" (quote (("name" . "outerWidth") ("signatures" "outerWidth" (("includeMargin" "A Boolean indicating whether to include the elementʼs margin in the
calculation.

" "true" nil))) ("desc" (text . "Get the current computed width for the first element in the set of
matched elements, including padding and border.

")) ("longdesc" (text . "Returns the width of the element, along with left and right padding,
border, and optionally margin, in pixels.

") (text . "") (text . "If includeMargin is omitted or false, the padding and border are
included in the calculation; if true, the margin is also included.

") (text . "") (text . "This method is not applicable to window and document objects; for
these, use .width() instead.

") (text . "") (text . "[0042_04_06.png]


")) ("examples" ((text . "") (text . "Get the outerWidth of a paragraph.

") (text . "") (js . "var p = $(\"p:first\");
			  $(\"p:last\").text( \"outerWidth:\" + p.outerWidth()+ \" , outerWidth(true):\" + p.outerWidth(true) );
			  
			  ") (text . "") (css . "
			  p { margin:10px;padding:5px;border:2px solid #666; }
			  ") (text . "") (html . "<p>Hello</p><p></p>") (text . ""))))) jquery-doc-hash)

(push "outerHeight" jquery-methods)

(puthash "outerHeight" (quote (("name" . "outerHeight") ("signatures" "outerHeight" (("includeMargin" "A Boolean indicating whether to include the elementʼs margin in the
calculation.

" "true" nil))) ("desc" (text . "Get the current computed height for the first element in the set of
matched elements, including padding, border, and optionally margin.

")) ("longdesc" (text . "The top and bottom padding and border are always included in the
.outerHeight() calculation; if the includeMargin argument is set to
true, the margin (top and bottom) is also included.
") (text . "") (text . "This method is not applicable to window and document objects; for
these, use .height() instead.

") (text . "") (text . "[0042_04_03.png]


")) ("examples" ((text . "") (text . "Get the outerHeight of a paragraph.

") (text . "") (js . "var p = $(\"p:first\");
			  $(\"p:last\").text( \"outerHeight:\" + p.outerHeight() + \" , outerHeight(true):\" + p.outerHeight(true) );") (text . "") (css . "p { margin:10px;padding:5px;border:2px solid #666; } ") (text . "") (html . "<p>Hello</p><p></p>") (text . ""))))) jquery-doc-hash)

(push "toggle" jquery-methods)

(puthash "toggle" (quote (("name" . "toggle") ("signatures" "toggle" (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("showOrHide" "A Boolean indicating whether to show or hide the elements.

" nil nil))) ("desc" (text . "Display or hide the matched elements.

")) ("longdesc" (text . "With no parameters, the .toggle() method simply toggles the visibility
of elements:

") (text . "") (js . "$('.target').toggle();
			") (text . "") (text . "The matched elements will be revealed or hidden immediately, with no
animation, by changing the CSS display property. If the element is
initially displayed, it will be hidden; if hidden, it will be shown.
The display property is saved and restored as needed. If an element has
a display value of inline, then is hidden and shown, it will once again
be displayed inline.
") (text . "") (text . "When a duration is provided, .toggle() becomes an animation method. The
.toggle() method animates the width, height, and opacity of the matched
elements simultaneously. When these properties reach 0 after a hiding
animation, the display style property is set to none to ensure that the
element no longer affects the layout of the page.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively.
") (text . "") (text . "  Note: The event handling suite also has a method named .toggle().
  Which one is fired depends on the set of arguments passed.

") (text . "") (text . "As of jQuery 1.4.3, an optional string naming an easing function may be
used. Easing functions specify the speed at which the animation
progresses at different points within the animation. The only easing
implementations in the jQuery library are the default, called swing,
and one that progresses at a constant pace, called linear. More easing
functions are available with the use of plug-ins, most notably the
jQuery UI suite.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
			Click here
			</div>
			<img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />
			") (text . "") (text . "We will cause .toggle() to be called when another element is clicked:


") (text . "") (js . "$('#clickme').click(function() {
			$('#book').toggle('slow', function() {
			// Animation complete.
			});
			});
			") (text . "") (text . "With the element initially shown, we can hide it slowly with the first
click:

") (text . "") (text . "

") (text . "") (text . "A second click will show the element once again:


") (text . "") (text . "

") (text . "") (text . "The second version of the method accepts a Boolean parameter. If this
parameter is true, then the matched elements are shown; if false, the
elements are hidden. In essence, the statement:
") (text . "") (js . "$('#foo').toggle(showOrHide);") (text . "") (text . "is equivalent to:


") (text . "") (js . "if ( showOrHide == true ) {
			$('#foo').show();
			} else if ( showOrHide == false ) {
			$('#foo').hide();
			}
			")) ("examples" ((text . "") (text . "Toggles all paragraphs.

") (text . "") (js . "

			  $(\"button\").click(function () {
			  $(\"p\").toggle();
			  });
			  ") (text . "") (html . "<button>Toggle</button>
			  <p>Hello</p>
			  <p style=\"display: none\">Good Bye</p>") (text . "")) ((text . "") (text . "Animates all paragraphs to be shown if they are hidden and hidden if
they are visible, completing the animation within 600 milliseconds.

") (text . "") (js . "
			  $(\"button\").click(function () {
			  $(\"p\").toggle(\"slow\");
			  });    
			  ") (text . "") (css . "
			  p { background:#dad;
			  font-weight:bold;
			  font-size:16px; }
			  ") (text . "") (html . "<button>Toggle 'em</button>

			  <p>Hiya</p>
			  <p>Such interesting text, eh?</p>") (text . "")) ((text . "") (text . "Shows all paragraphs, then hides them all, back and forth.

") (text . "") (js . "

			  var flip = 0;
			  $(\"button\").click(function () {
			  $(\"p\").toggle( flip++ % 2 == 0 );
			  });
			  ") (text . "") (html . "<button>Toggle</button>
			  <p>Hello</p>
			  <p style=\"display: none\">Good Bye</p>") (text . ""))))) jquery-doc-hash)

(push "innerWidth" jquery-methods)

(puthash "innerWidth" (quote (("name" . "innerWidth") ("signatures" "innerWidth" nil) ("desc" (text . "Get the current computed width for the first element in the set of
matched elements, including padding but not border.

")) ("longdesc" (text . "This method returns the width of the element, including left and right
padding, in pixels.

") (text . "") (text . "This method is not applicable to window and document objects; for
these, use .width() instead.

") (text . "") (text . "[0042_04_05.png]


")) ("examples" ((text . "") (text . "Get the innerWidth of a paragraph.

") (text . "") (js . "var p = $(\"p:first\");
			    $(\"p:last\").text( \"innerWidth:\" + p.innerWidth() );") (text . "") (css . "p { margin:10px;padding:5px;border:2px solid #666; } ") (text . "") (html . "<p>Hello</p><p></p>") (text . ""))))) jquery-doc-hash)

(push "innerHeight" jquery-methods)

(puthash "innerHeight" (quote (("name" . "innerHeight") ("signatures" "innerHeight" nil) ("desc" (text . "Get the current computed height for the first element in the set of
matched elements, including padding but not border.

")) ("longdesc" (text . "This method returns the height of the element, including top and bottom
padding, in pixels.

") (text . "") (text . "This method is not applicable to window and document objects; for
these, use .height() instead.

") (text . "") (text . "[0042_04_02.png]


")) ("examples" ((text . "") (text . "Get the innerHeight of a paragraph.

") (text . "") (js . "var p = $(\"p:first\");
			    $(\"p:last\").text( \"innerHeight:\" + p.innerHeight() );") (text . "") (css . "p { margin:10px;padding:5px;border:2px solid #666; }") (text . "") (html . "<p>Hello</p><p></p>") (text . ""))))) jquery-doc-hash)

(push "$.param" jquery-methods)

(puthash "$.param" (quote (("name" . "$.param") ("signatures" "$.param" (("obj" "An array or object to serialize.

" nil nil)) (("obj" "An array or object to serialize.

" nil nil) ("traditional" "A Boolean indicating whether to perform a traditional \"shallow\"
serialization.

" nil nil))) ("desc" (text . "Create a serialized representation of an array or object, suitable for
use in a URL query string or Ajax request.

")) ("longdesc" (text . "") (text . "This function is used internally to convert form element values into a
serialized string representation (See .serialize() for more
information).
") (text . "") (text . "As of jQuery 1.3, the return value of a function is used instead of the
function as a String.

") (text . "") (text . "As of jQuery 1.4, the $.param() method serializes deep objects
recursively to accommodate modern scripting languages and frameworks
such as PHP and Ruby on Rails. You can disable this functionality
globally by setting jQuery.ajaxSettings.traditional = true;.
") (text . "") (text . "If the object passed is in an Array, it must be an array of objects in
the format returned by .serializeArray()

") (text . "") (js . "[{name:\"first\",value:\"Rick\"},
			    {name:\"last\",value:\"Astley\"},
			    {name:\"job\",value:\"Rock Star\"}]") (text . "") (text . "Note: Because some frameworks have limited ability to parse serialized
arrays, we should exercise caution when passing an obj argument that
contains objects or arrays nested within another array.
") (text . "") (text . "In jQuery 1.4 HTML5 input elements are serialized, as well.


") (text . "") (text . "We can display a query string representation of an object and a
URI-decoded version of the same as follows:

") (text . "") (js . "var myObject = {
			    a: {
			    one: 1, 
			    two: 2, 
			    three: 3
			    }, 
			    b: [1,2,3]
			    };
			    var recursiveEncoded = $.param(myObject);
			    var recursiveDecoded = decodeURIComponent($.param(myObject));
			    
			    alert(recursiveEncoded);
			    alert(recursiveDecoded);
			    ") (text . "") (text . "The values of recursiveEncoded and recursiveDecoded are alerted as
follows:

") (text . "") (text . "a%5Bone%5D=1&a%5Btwo%5D=2&a%5Bthree%5D=3&b%5B%5D=1&b%5B%5D=2&b%5B%5D=3
a[one]=1&a[two]=2&a[three]=3&b[]=1&b[]=2&b[]=3

") (text . "") (text . "To emulate the behavior of $.param() prior to jQuery 1.4, we can set
the traditional argument to true:

") (text . "") (js . "var myObject = {
			    a: {
			    one: 1, 
			    two: 2, 
			    three: 3
			    }, 
			    b: [1,2,3]
			    };
			    var shallowEncoded = $.param(myObject, true);
			    var shallowDecoded = decodeURIComponent(shallowEncoded);

			    alert(shallowEncoded);
			    alert(shallowDecoded);
			    ") (text . "") (text . "The values of shallowEncoded and shallowDecoded are alerted as follows:


") (text . "") (text . "a=%5Bobject+Object%5D&b=1&b=2&b=3
a=[object+Object]&b=1&b=2&b=3

") (text . "")) ("examples" ((text . "") (text . "Serialize a key/value object.

") (text . "") (js . "

			    var params = { width:1680, height:1050 };
			    var str = jQuery.param(params);
			    $(\"#results\").text(str);
			    ") (text . "") (css . "div { color:red; }") (text . "") (html . "<div id=\"results\"></div>") (text . "")) ((text . "") (text . "Serialize a few complex objects

") (text . "") (js . "
			    // <=1.3.2: 
			    $.param({ a: [2,3,4] }) // \"a=2&a=3&a=4\"
			    // >=1.4:
			    $.param({ a: [2,3,4] }) // \"a[]=2&a[]=3&a[]=4\"

			    // <=1.3.2: 
			    $.param({ a: { b:1,c:2 }, d: [3,4,{ e:5 }] }) // \"a=[object+Object]&d=3&d=4&d=[object+Object]\"
			    // >=1.4: 
			    $.param({ a: { b:1,c:2 }, d: [3,4,{ e:5 }] }) // \"a[b]=1&a[c]=2&d[]=3&d[]=4&d[2][e]=5\"

			    ") (text . "") (css . "div { color:red; }") (text . ""))))) jquery-doc-hash)

(push "hide" jquery-methods)

(puthash "hide" (quote (("name" . "hide") ("signatures" "hide" nil (("duration" "A string or number determining how long the animation will run.

" nil nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Hide the matched elements.

")) ("longdesc" (text . "") (text . "With no parameters, the .hide() method is the simplest way to hide an
element:

") (text . "") (js . "$('.target').hide();
			    ") (text . "") (text . "The matched elements will be hidden immediately, with no animation.
This is roughly equivalent to calling .css(ʼdisplayʼ, ʼnoneʼ), except
that the value of the display property is saved in jQueryʼs data cache
so that display can later be restored to its initial value. If an
element has a display value of inline, then is hidden and shown, it
will once again be displayed inline.
") (text . "") (text . "When a duration is provided, .hide() becomes an animation method. The
.hide() method animates the width, height, and opacity of the matched
elements simultaneously. When these properties reach 0, the display
style property is set to none to ensure that the element no longer
affects the layout of the page.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively.
") (text . "") (text . "As of jQuery 1.4.3, an optional string naming an easing function may be
used. Easing functions specify the speed at which the animation
progresses at different points within the animation. The only easing
implementations in the jQuery library are the default, called swing,
and one that progresses at a constant pace, called linear. More easing
functions are available with the use of plug-ins, most notably the
jQuery UI suite.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
			    Click here
			    </div>
			    <img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />
			    With the element initially shown, we can hide it slowly:
			    $('#clickme').click(function() {
			    $('#book').hide('slow', function() {
			    alert('Animation complete.');
			    });
			    });") (text . "") (text . "

") (text . "")) ("examples" ((text . "") (text . "Hides all paragraphs then the link on click.

") (text . "") (js . "

			    $(\"p\").hide();
			    $(\"a\").click(function () {
			    $(this).hide();
			    return true;
			    });
			    ") (text . "") (html . "<p>Hello</p>
			    <a href=\"#\">Click to hide me too</a>
			    <p>Here is another paragraph</p>") (text . "")) ((text . "") (text . "Animates all shown paragraphs to hide slowly, completing the animation
within 600 milliseconds.

") (text . "") (js . "
			    $(\"button\").click(function () {
			    $(\"p\").hide(\"slow\");
			    });    
			    ") (text . "") (css . "
			    p { background:#dad; font-weight:bold; }
			    ") (text . "") (html . "<button>Hide 'em</button>

			    <p>Hiya</p>
			    <p>Such interesting text, eh?</p>") (text . "")) ((text . "") (text . "Animates all spans (words in this case) to hide fastly, completing each
animation within 200 milliseconds. Once each animation is done, it
starts the next one.
") (text . "") (js . "
			    $(\"#hidr\").click(function () {
			    $(\"span:last-child\").hide(\"fast\", function () {
			    // use callee so don't have to name the function
			    $(this).prev().hide(\"fast\", arguments.callee); 
			    });
			    });
			    $(\"#showr\").click(function () {
			    $(\"span\").show(2000);
			    });

			    ") (text . "") (css . "
			    span { background:#def3ca; padding:3px; float:left; }
			    ") (text . "") (html . "<button id=\"hidr\">Hide</button>
			    <button id=\"showr\">Show</button>
			    <div>

			    <span>Once</span> <span>upon</span> <span>a</span> 
			    <span>time</span> <span>there</span> <span>were</span> 
			    <span>three</span> <span>programmers...</span>

			    </div>") (text . "")) ((text . "") (text . "Hides the divs when clicked over 2 seconds, then removes the div
element when its hidden. Try clicking on more than one box at a time.

") (text . "") (js . "
			    for (var i = 0; i < 5; i++) {
			    $(\"<div>\").appendTo(document.body);
			    }
			    $(\"div\").click(function () {
			    $(this).hide(2000, function () {
			    $(this).remove();
			    });
			    });
			    ") (text . "") (css . "
			    div { background:#ece023; width:30px; 
			    height:40px; margin:2px; float:left; }
			    ") (text . "") (html . "<div></div>") (text . ""))))) jquery-doc-hash)

(push "width" jquery-methods)

(puthash "width" (quote (("name" . "width") ("signatures" "width" nil) ("desc" (text . "Get the current computed width for the first element in the set of
matched elements.

")) ("longdesc" (text . "The difference between .css(width) and .width() is that the latter
returns a unit-less pixel value (for example, 400) while the former
returns a value with units intact (for example, 400px). The .width()
method is recommended when an elementʼs width needs to be used in a
mathematical calculation.
") (text . "") (text . "[0042_04_04.png]


") (text . "") (text . "This method is also able to find the width of the window and document.


") (text . "") (js . "$(window).width();   // returns width of browser viewport
			    $(document).width(); // returns width of HTML document") (text . "") (text . "Note that .width() will always return the content width, regardless of
the value of the CSS box-sizing property.

")) ("examples" ((text . "") (text . "Show various widths. Note the values are from the iframe so might be
smaller than you expected. The yellow highlight shows the iframe body.

") (text . "") (js . "
			      function showWidth(ele, w) {
			      $(\"div\").text(\"The width for the \" + ele + 
			      \" is \" + w + \"px.\");
			      }
			      $(\"#getp\").click(function () { 
			      showWidth(\"paragraph\", $(\"p\").width()); 
			      });
			      $(\"#getd\").click(function () { 
			      showWidth(\"document\", $(document).width()); 
			      });
			      $(\"#getw\").click(function () { 
			      showWidth(\"window\", $(window).width()); 
			      });
			      
			      ") (text . "") (css . "
			      body { background:yellow; }
			      button { font-size:12px; margin:2px; }
			      p { width:150px; border:1px red solid; }
			      div { color:red; font-weight:bold;  }
			      ") (text . "") (html . "<button id=\"getp\">Get Paragraph Width</button>
			      <button id=\"getd\">Get Document Width</button>
			      <button id=\"getw\">Get Window Width</button>
			      
			      <div>&nbsp;</div>
			      <p>
			      Sample paragraph to test width
			      </p>") (text . ""))))) jquery-doc-hash)

(push "width" jquery-methods)

(puthash "width" (quote (("name" . "width") ("signatures" "width" (("value" "An integer representing the number of pixels, or an integer along with
an optional unit of measure appended (as a string).

" nil nil)) (("function(index, width)" "A function returning the width to set. Receives the index position of
the element in the set and the old width as arguments.

" nil nil))) ("desc" (text . "Set the CSS width of each element in the set of matched elements.

")) ("longdesc" (text . "When calling .width(ʼvalueʼ), the value can be either a string (number
and unit) or a number. If only a number is provided for the value,
jQuery assumes a pixel unit. If a string is provided, however, any
valid CSS measurement may be used for the width (such as 100px, 50%, or
auto). Note that in modern browsers, the CSS width property does not
include padding, border, or margin, unless the box-sizing CSS property
is used.
") (text . "") (text . "If no explicit unit was specified (like ʼemʼ or ʼ%ʼ) then \"px\" is
concatenated to the value.

") (text . "") (text . "Note that .width(ʼvalueʼ) sets the width of the box in accordance with
the CSS box-sizing property. Changing this property to border-box will
cause this function to change the outerWidth of the box instead of the
content width.
")) ("examples" ((text . "") (text . "To set the width of each div on click to 30px plus a color change.

") (text . "") (js . "
			      
			      $(\"div\").one('click', function () {
			      $(this).width(30)
			      .css({cursor:\"auto\", \"background-color\":\"blue\"});
			      });
			      ") (text . "") (css . "
			      div { width:70px; height:50px; float:left; margin:5px;
			      background:red; cursor:pointer; }
			      ") (text . "") (html . "<div></div>
			      <div>d</div>
			      
			      <div>d</div>
			      <div>d</div>
			      <div>d</div>") (text . ""))))) jquery-doc-hash)

(push "height" jquery-methods)

(puthash "height" (quote (("name" . "height") ("signatures" "height" nil) ("desc" (text . "Get the current computed height for the first element in the set of
matched elements.

")) ("longdesc" (text . "The difference between .css(ʼheightʼ) and .height() is that the latter
returns a unit-less pixel value (for example, 400) while the former
returns a value with units intact (for example, 400px). The .height()
method is recommended when an elementʼs height needs to be used in a
mathematical calculation.
") (text . "") (text . "[0042_04_01.png]


") (text . "") (text . "This method is also able to find the height of the window and document.


") (text . "") (js . "$(window).height();   // returns height of browser viewport
			    $(document).height(); // returns height of HTML document") (text . "") (text . "Note that .height() will always return the content height, regardless
of the value of the CSS box-sizing property.

")) ("examples" ((text . "") (text . "Show various heights. Note the values are from the iframe so might be
smaller than you expected. The yellow highlight shows the iframe body.

") (text . "") (js . "
			      function showHeight(ele, h) {
			      $(\"div\").text(\"The height for the \" + ele + 
			      \" is \" + h + \"px.\");
			      }
			      $(\"#getp\").click(function () { 
			      showHeight(\"paragraph\", $(\"p\").height()); 
			      });
			      $(\"#getd\").click(function () { 
			      showHeight(\"document\", $(document).height()); 
			      });
			      $(\"#getw\").click(function () { 
			      showHeight(\"window\", $(window).height()); 
			      });
			      
			      ") (text . "") (css . "
			      body { background:yellow; }
			      button { font-size:12px; margin:2px; }
			      p { width:150px; border:1px red solid; }
			      div { color:red; font-weight:bold; }
			      ") (text . "") (html . "<button id=\"getp\">Get Paragraph Height</button>
			      <button id=\"getd\">Get Document Height</button>
			      <button id=\"getw\">Get Window Height</button>
			      
			      <div>&nbsp;</div>
			      <p>
			      Sample paragraph to test height
			      </p>") (text . ""))))) jquery-doc-hash)

(push "height" jquery-methods)

(puthash "height" (quote (("name" . "height") ("signatures" "height" (("value" "An integer representing the number of pixels, or an integer with an
optional unit of measure appended (as a string).

" nil nil)) (("function(index, height)" "A function returning the height to set. Receives the index position of
the element in the set and the old height as arguments.

" nil nil))) ("desc" (text . "Set the CSS height of every matched element.

")) ("longdesc" (text . "When calling .height(value), the value can be either a string (number
and unit) or a number. If only a number is provided for the value,
jQuery assumes a pixel unit. If a string is provided, however, any
valid CSS measurement may be used for the height (such as 100px, 50%,
or auto). Note that in modern browsers, the CSS height property does
not include padding, border, or margin.
") (text . "") (text . "If no explicit unit was specified (like ʼemʼ or ʼ%ʼ) then \"px\" is
concatenated to the value.

") (text . "") (text . "Note that .height(value) sets the height of the box in accordance with
the CSS box-sizing property. Changing this property to border-box will
cause this function to change the outerHeight of the box instead of the
content height.
")) ("examples" ((text . "") (text . "To set the height of each div on click to 30px plus a color change.

") (text . "") (js . "$(\"div\").one('click', function () {
			      $(this).height(30)
			      .css({cursor:\"auto\", backgroundColor:\"green\"});
			      });") (text . "") (css . "div { width:50px; height:70px; float:left; margin:5px;
			      background:rgb(255,140,0); cursor:pointer; }  ") (text . "") (html . "<div></div>
			      <div></div>
			      
			      <div></div>
			      <div></div>
			      <div></div>") (text . ""))))) jquery-doc-hash)

(push "show" jquery-methods)

(puthash "show" (quote (("name" . "show") ("signatures" "show" nil (("duration" "A string or number determining how long the animation will run.

" nil nil) ("callback" "A function to call once the animation is complete.

" "true" nil)) (("duration" "A string or number determining how long the animation will run.

" "true" nil) ("easing" "A string indicating which easing function to use for the transition.

" "true" nil) ("callback" "A function to call once the animation is complete.

" "true" nil))) ("desc" (text . "Display the matched elements.

")) ("longdesc" (text . "") (text . "With no parameters, the .show() method is the simplest way to display
an element:

") (text . "") (js . "$('.target').show();
			      ") (text . "") (text . "The matched elements will be revealed immediately, with no animation.
This is roughly equivalent to calling .css(ʼdisplayʼ, ʼblockʼ), except
that the display property is restored to whatever it was initially. If
an element has a display value of inline, then is hidden and shown, it
will once again be displayed inline.
") (text . "") (text . "Note: If using !important in your styles, such as display: none
!important, it is necessary to override the style using .css(ʼdisplayʼ,
ʼblock !importantʼ) should you wish for .show() to function correctly.
") (text . "") (text . "When a duration is provided, .show() becomes an animation method. The
.show() method animates the width, height, and opacity of the matched
elements simultaneously.
") (text . "") (text . "Durations are given in milliseconds; higher values indicate slower
animations, not faster ones. The strings ʼfastʼ and ʼslowʼ can be
supplied to indicate durations of 200 and 600 milliseconds,
respectively.
") (text . "") (text . "As of jQuery 1.4.3, an optional string naming an easing function may be
used. Easing functions specify the speed at which the animation
progresses at different points within the animation. The only easing
implementations in the jQuery library are the default, called swing,
and one that progresses at a constant pace, called linear. More easing
functions are available with the use of plug-ins, most notably the
jQuery UI suite.
") (text . "") (text . "If supplied, the callback is fired once the animation is complete. This
can be useful for stringing different animations together in sequence.
The callback is not sent any arguments, but this is set to the DOM
element being animated. If multiple elements are animated, it is
important to note that the callback is executed once per matched
element, not once for the animation as a whole.
") (text . "") (text . "We can animate any element, such as a simple image:


") (text . "") (js . "<div id=\"clickme\">
			      Click here
			      </div>
			      <img id=\"book\" src=\"book.png\" alt=\"\" width=\"100\" height=\"123\" />
			      With the element initially hidden, we can show it slowly:
			      $('#clickme').click(function() {
			      $('#book').show('slow', function() {
			      // Animation complete.
			      });
			      });") (text . "") (text . "

") (text . "")) ("examples" ((text . "") (text . "Animates all hidden paragraphs to show slowly, completing the animation
within 600 milliseconds.

") (text . "") (js . "
			      $(\"button\").click(function () {
			      $(\"p\").show(\"slow\");
			      });
			      ") (text . "") (css . "
			      p { background:yellow; }
			      ") (text . "") (html . "<button>Show it</button>

			      <p style=\"display: none\">Hello  2</p>") (text . "")) ((text . "") (text . "Animates all hidden divs to show fastly in order, completing each
animation within 200 milliseconds. Once each animation is done, it
starts the next one.
") (text . "") (js . "
			      $(\"#showr\").click(function () {
			      $(\"div:eq(0)\").show(\"fast\", function () {
			      /* use callee so don't have to name the function */
			      $(this).next(\"div\").show(\"fast\", arguments.callee);
			      });
			      });
			      $(\"#hidr\").click(function () {
			      $(\"div\").hide(2000);
			      });

			      ") (text . "") (css . "
			      div { background:#def3ca; margin:3px; width:80px; 
			      display:none; float:left; text-align:center; }
			      ") (text . "") (html . "
			      <button id=\"showr\">Show</button>
			      <button id=\"hidr\">Hide</button>
			      <div>Hello 3,</div>

			      <div>how</div>
			      <div>are</div>
			      <div>you?</div>") (text . "")) ((text . "") (text . "Shows all span and input elements with an animation. Once the animation
is done, it changes the text.

") (text . "") (js . "
			      function doIt() {
			      $(\"span,div\").show(\"slow\");
			      }
			      /* can pass in function name */
			      $(\"button\").click(doIt);

			      $(\"form\").submit(function () {
			      if ($(\"input\").val() == \"yes\") {
			      $(\"p\").show(4000, function () {
			      $(this).text(\"Ok, DONE! (now showing)\");
			      });
			      }
			      $(\"span,div\").hide(\"fast\");
			      /* to stop the submit */
			      return false; 
			      });
			      ") (text . "") (css . "
			      span { display:none; }
			      div { display:none; }
			      p { font-weight:bold; background-color:#fcd; }
			      ") (text . "") (html . "<button>Do it!</button>
			      <span>Are you sure? (type 'yes' if you are) </span>
			      <div>
			      <form>
			      <input type=\"text\"  value=\"as;ldkfjalsdf\"/>
			      </form>
			      </div>
			      <p style=\"display:none;\">I'm hidden...</p>
			      ") (text . ""))))) jquery-doc-hash)

(push "scrollLeft" jquery-methods)

(puthash "scrollLeft" (quote (("name" . "scrollLeft") ("signatures" "scrollLeft" nil) ("desc" (text . "Get the current horizontal position of the scroll bar for the first
element in the set of matched elements.

")) ("longdesc" (text . "The horizontal scroll position is the same as the number of pixels that
are hidden from view above the scrollable area. If the scroll bar is at
the very left, or if the element is not scrollable, this number will be
0.
")) ("examples" ((text . "") (text . "Get the scrollLeft of a paragraph.

") (text . "") (js . "var p = $(\"p:first\");
				$(\"p:last\").text( \"scrollLeft:\" + p.scrollLeft() );
				
				") (text . "") (css . "
				p { margin:10px;padding:5px;border:2px solid #666; }
				") (text . "") (html . "<p>Hello</p><p></p>") (text . ""))))) jquery-doc-hash)

(push "scrollLeft" jquery-methods)

(puthash "scrollLeft" (quote (("name" . "scrollLeft") ("signatures" "scrollLeft" (("value" "An integer indicating the new position to set the scroll bar to.

" nil nil))) ("desc" (text . "Set the current horizontal position of the scroll bar for each of the
set of matched elements.

")) ("longdesc" (text . "The horizontal scroll position is the same as the number of pixels that
are hidden from view above the scrollable area. Setting the scrollLeft
positions the horizontal scroll of each matched element.
")) ("examples" ((text . "") (text . "Set the scrollLeft of a div.

") (text . "") (js . "$(\"div.demo\").scrollLeft(300);
				") (text . "") (css . "
				div.demo {
				background:#CCCCCC none repeat scroll 0 0;
				border:3px solid #666666;
				margin:5px;
				padding:5px;
				position:relative;
				width:200px;
				height:100px;
				overflow:auto;
				}
				p { margin:10px;padding:5px;border:2px solid #666;width:1000px;height:1000px; }
				") (text . "") (html . "<div class=\"demo\"><h1>lalala</h1><p>Hello</p></div>") (text . ""))))) jquery-doc-hash)

(push "$.trim" jquery-methods)

(puthash "$.trim" (quote (("name" . "$.trim") ("signatures" "$.trim" (("str" "The string to trim.

" nil nil))) ("desc" (text . "Remove the whitespace from the beginning and end of a string.

")) ("longdesc" (text . "") (text . "The $.trim() function removes all newlines, spaces (including
non-breaking spaces), and tabs from the beginning and end of the
supplied string. If these whitespace characters occur in the middle of
the string, they are preserved.
") (text . "")) ("examples" ((text . "") (text . "Remove the two white spaces at the start and at the end of the string.

") (text . "") (js . "
				
				$(\"button\").click(function () {
				var str = \"     lots of spaces before and after     \";
				alert(\"'\" + str + \"'\");
				
				str = jQuery.trim(str);
				alert(\"'\" + str + \"' - no longer\");
				});
				
				") (text . "") (html . "<button>Show Trim Example</button>") (text . "")) ((text . "") (text . "Remove the two white spaces at the start and at the end of the string.

") (text . "") (js . "$.trim(\"  hello, how are you?  \");") (text . "") (text . "\"hello, how are you?\"

") (text . ""))))) jquery-doc-hash)

(push "$.isFunction" jquery-methods)

(puthash "$.isFunction" (quote (("name" . "$.isFunction") ("signatures" "$.isFunction" (("obj" "Object to test whether or not it is a function.

" nil nil))) ("desc" (text . "Determine if the argument passed is a Javascript function object.

")) ("longdesc" (text . "Note: As of jQuery 1.3, functions provided by the browser like alert()
and DOM element methods like getAttribute() are not guaranteed to be
detected as functions in browsers such as Internet Explorer.
")) ("examples" ((text . "") (text . "Test a few parameter examples.

") (text . "") (js . "
				function stub() {
				}
				var objs = [
				function () {},
				{ x:15, y:20 },
				null,
				stub,
				\"function\"
				];
				
				jQuery.each(objs, function (i) {
				var isFunc = jQuery.isFunction(objs[i]);
				$(\"span\").eq(i).text(isFunc);
				});
				") (text . "") (css . "
				div { color:blue; margin:2px; font-size:14px; }
				span { color:red; }
				") (text . "") (html . "
				<div>jQuery.isFunction(objs[0]) = <span></span></div>
				
				<div>jQuery.isFunction(objs[1]) = <span></span></div>
				<div>jQuery.isFunction(objs[2]) = <span></span></div>
				<div>jQuery.isFunction(objs[3]) = <span></span></div>
				
				<div>jQuery.isFunction(objs[4]) = <span></span></div>
				") (text . "")) ((text . "") (text . "Finds out if the parameter is a funcion.

") (text . "") (js . "$.isFunction(function(){});") (text . "") (text . "true

") (text . ""))))) jquery-doc-hash)

(push "$.isArray" jquery-methods)

(puthash "$.isArray" (quote (("name" . "$.isArray") ("signatures" "$.isArray" (("obj" "Object to test whether or not it is an array.

" nil nil))) ("desc" (text . "Determine whether the argument is an array.

")) ("longdesc" (text . "$.isArray() returns a Boolean indicating whether the object is a
JavaScript array (not an array-like object, such as a jQuery object).

")) ("examples" ((text . "") (text . "Finds out if the parameter is an array.

") (text . "") (js . "$(\"b\").append( \"\" + $.isArray([]) );") (text . "") (html . "Is [] an Array? <b></b>") (text . ""))))) jquery-doc-hash)

(push "$.unique" jquery-methods)

(puthash "$.unique" (quote (("name" . "$.unique") ("signatures" "$.unique" (("array" "The Array of DOM elements.

" nil nil))) ("desc" (text . "Sorts an array of DOM elements, in place, with the duplicates removed.
Note that this only works on arrays of DOM elements, not strings or
numbers.
")) ("longdesc" (text . "The $.unique() function searches through an array of objects, sorting
the array, and removing any duplicate nodes. This function only works
on plain JavaScript arrays of DOM elements, and is chiefly used
internally by jQuery.
") (text . "") (text . "As of jQuery 1.4 the results will always be returned in document order.


")) ("examples" ((text . "") (text . "Removes any duplicate elements from the array of divs.

") (text . "") (js . "
				
				var divs = $(\"div\").get(); // unique() must take a native array
				
				// add 3 elements of class dup too (they are divs)
				divs = divs.concat($(\".dup\").get());
				$(\"div:eq(1)\").text(\"Pre-unique there are \" + divs.length + \" elements.\");
				
				divs = jQuery.unique(divs);
				$(\"div:eq(2)\").text(\"Post-unique there are \" + divs.length + \" elements.\")
				.css(\"color\", \"red\");
				
				") (text . "") (css . "
				div { color:blue; }
				") (text . "") (html . "<div>There are 6 divs in this document.</div>
				<div></div>
				<div class=\"dup\"></div>
				<div class=\"dup\"></div>
				
				<div class=\"dup\"></div>
				<div></div>") (text . ""))))) jquery-doc-hash)

(push "$.merge" jquery-methods)

(puthash "$.merge" (quote (("name" . "$.merge") ("signatures" "$.merge" (("first" "The first array to merge, the elements of second added.

" nil nil) ("second" "The second array to merge into the first, unaltered.

" nil nil))) ("desc" (text . "Merge the contents of two arrays together into the first array.

")) ("longdesc" (text . "The $.merge() operation forms an array that contains all elements from
the two arrays. The orders of items in the arrays are preserved, with
items from the second array appended. The $.merge() function is
destructive. It alters the first parameter to add the items from the
second.
") (text . "") (text . "If you need the original first array, make a copy of it before calling
$.merge(). Fortunately, $.merge() itself can be used for this
duplication:
") (text . "") (js . "var newArray = $.merge([], oldArray);") (text . "") (text . "This shortcut creates a new, empty array and merges the contents of
oldArray into it, effectively cloning the array.

") (text . "") (text . "Prior to jQuery 1.4, the arguments should be true Javascript Array
objects; use $.makeArray if they are not.

")) ("examples" ((text . "") (text . "Merges two arrays, altering the first argument.

") (text . "") (js . "$.merge( [0,1,2], [2,3,4] )") (text . "") (text . "[0,1,2,2,3,4]

") (text . "")) ((text . "") (text . "Merges two arrays, altering the first argument.

") (text . "") (js . "$.merge( [3,2,1], [4,3,2] )  ") (text . "") (text . "[3,2,1,4,3,2]

") (text . "")) ((text . "") (text . "Merges two arrays, but uses a copy, so the original isnʼt altered.

") (text . "") (js . "var first = ['a','b','c'];
				var second = ['d','e','f'];
				$.merge( $.merge([],first), second);
				") (text . "") (text . "[\"a\",\"b\",\"c\",\"d\",\"e\",\"f\"]

") (text . ""))))) jquery-doc-hash)

(push "$.inArray" jquery-methods)

(puthash "$.inArray" (quote (("name" . "$.inArray") ("signatures" "$.inArray" (("value" "The value to search for.

" nil nil) ("array" "An array through which to search.

" nil nil))) ("desc" (text . "Search for a specified value within an array and return its index (or
-1 if not found).

")) ("longdesc" (text . "The $.inArray() method is similar to JavaScriptʼs native .indexOf()
method in that it returns -1 when it doesnʼt find a match. If the first
element within the array matches value, $.inArray() returns 0.
") (text . "") (text . "Because JavaScript treats 0 as loosely equal to false (i.e. 0 == false,
but 0 !== false), if weʼre checking for the presence of value within
array, we need to check if itʼs not equal to (or greater than) -1.
") (text . "")) ("examples" ((text . "") (text . "Report the index of some elements in the array.

") (text . "") (js . "var arr = [ 4, \"Pete\", 8, \"John\" ];

				$(\"span:eq(0)\").text(jQuery.inArray(\"John\", arr));
				$(\"span:eq(1)\").text(jQuery.inArray(4, arr));
				$(\"span:eq(2)\").text(jQuery.inArray(\"Karl\", arr));

				") (text . "") (css . "
				div { color:blue; }
				span { color:red; }
				") (text . "") (html . "
				<div>\"John\" found at <span></span></div>
				<div>4 found at <span></span></div>
				<div>\"Karl\" not found, so <span></span></div>") (text . ""))))) jquery-doc-hash)

(push "$.map" jquery-methods)

(puthash "$.map" (quote (("name" . "$.map") ("signatures" "$.map" (("array" "The Array to translate.

" nil nil) ("callback(elementOfArray, indexInArray)" "The function to process each item against. The first argument to the
function is the array item, the second argument is the index in array
The function can return any value. Within the function, this refers to
the global (window) object.
" nil nil)) (("arrayOrObject" "The Array or Object to translate.

" nil nil) ("callback( value, indexOrKey )" "The function to process each item against. The first argument to the
function is the value; the second argument is the index or key of the
array or object property. The function can return any value to add to
the array. A returned array will be flattened into the resulting array.
Within the function, this refers to the global (window) object.
" nil nil))) ("desc" (text . "Translate all items in an array or object to new array of items.

")) ("longdesc" (text . "") (text . "The $.map() method applies a function to each item in an array or
object and maps the results into a new array. Prior to jQuery 1.6,
$.map() supports traversing arrays and array-like objects only. As of
jQuery 1.6 it also traverses objects.
") (text . "") (text . "Array-like objects, such as jQuery collections, are treated as arrays.
In other words, if an object has a .length property and a value on the
.length - 1 index, it is traversed as an array.
") (text . "") (text . "The translation function that is provided to this method is called for
each top-level element in the array or object and is passed two
arguments: The elementʼs value and its index or key within the array or
object.
") (text . "") (text . "The function can return:


") (text . "") (text . "  * the translated value, which will be mapped to the resulting array
  * null, to remove the item
  * an array of values, which will be flattened into the full array
") (text . "")) ("examples" ((text . "") (text . "A couple examples of using .map()

") (text . "") (js . "
				var arr = [ \"a\", \"b\", \"c\", \"d\", \"e\" ];
				$(\"div\").text(arr.join(\", \"));
				
				arr = jQuery.map(arr, function(n, i){
				return (n.toUpperCase() + i);
				});
				$(\"p\").text(arr.join(\", \"));
				
				arr = jQuery.map(arr, function (a) { 
				return a + a; 
				});
				$(\"span\").text(arr.join(\", \"));
				
				") (text . "") (css . "
				div { color:blue; }
				p { color:green; margin:0; }
				span { color:red; }
				") (text . "") (html . "<div></div>
				<p></p>
				<span></span>
				") (text . "")) ((text . "") (text . "Map the original array to a new one and add 4 to each value.

") (text . "") (js . "$.map( [0,1,2], function(n){
				return n + 4;
				});") (text . "") (text . "[4, 5, 6]

") (text . "")) ((text . "") (text . "Maps the original array to a new one and adds 1 to each value if it is
bigger then zero, otherwise itʼs removed.

") (text . "") (js . "$.map( [0,1,2], function(n){
				return n > 0 ? n + 1 : null;
				});") (text . "") (text . "[2, 3]

") (text . "")) ((text . "") (text . "Map the original array to a new one; each element is added with its
original value and the value plus one.

") (text . "") (js . "$.map( [0,1,2], function(n){
				return [ n, n + 1 ];
				});") (text . "") (text . "[0, 1, 1, 2, 2, 3]

") (text . "")) ((text . "") (text . "Map the original object to a new array and double each value.

") (text . "") (js . "
				var dimensions = { width: 10, height: 15, length: 20 };
				dimensions = $.map( dimensions, function( value, index ) {
				return value * 2;
				}); ") (text . "") (text . "[20, 30, 40]

") (text . "")) ((text . "") (text . "Map an objectʼs keys to an array.

") (text . "") (js . "
				var dimensions = { width: 10, height: 15, length: 20 },
				keys = $.map( dimensions, function( value, index ) {
				return index;
				}); ") (text . "") (text . "[\"width\", \"height\", \"length\"]

") (text . "")) ((text . "") (text . "Maps the original array to a new one; each element is squared.

") (text . "") (js . "
				$.map( [0,1,2,3], function (a) { 
				return a * a; 
				});") (text . "") (text . "[0, 1, 4, 9]

") (text . "")) ((text . "") (text . "Remove items by returning null from the function. This removes any
numbers less than 50, and the rest are decreased by 45.

") (text . "") (js . "
				$.map( [0, 1, 52, 97], function (a) {
				return (a > 50 ? a - 45 : null); 
				});") (text . "") (text . "[7, 52]

") (text . "")) ((text . "") (text . "Augmenting the resulting array by returning an array inside the
function.

") (text . "") (js . "var array = [0, 1, 52, 97];
				array = $.map(array, function(a, index) {
				return [a - 45, index];
				}); ") (text . "") (text . "[-45, 0, -44, 1, 7, 2, 52, 3]

") (text . ""))))) jquery-doc-hash)

(push "$.makeArray" jquery-methods)

(puthash "$.makeArray" (quote (("name" . "$.makeArray") ("signatures" "$.makeArray" (("obj" "Any object to turn into a native Array.

" nil nil))) ("desc" (text . "Convert an array-like object into a true JavaScript array.

")) ("longdesc" (text . "") (text . "Many methods, both in jQuery and in JavaScript in general, return
objects that are array-like. For example, the jQuery factory function
$() returns a jQuery object that has many of the properties of an array
(a length, the [] array access operator, etc.), but is not exactly the
same as an array and lacks some of an arrayʼs built-in methods (such as
.pop() and .reverse()).
") (text . "") (text . "Note that after the conversion, any special features the object had
(such as the jQuery methods in our example) will no longer be present.
The object is now a plain array.
")) ("examples" ((text . "") (text . "Turn a collection of HTMLElements into an Array of them.

") (text . "") (js . "
				var elems = document.getElementsByTagName(\"div\"); // returns a nodeList
				var arr = jQuery.makeArray(elems);
				arr.reverse(); // use an Array method on list of dom elements
				$(arr).appendTo(document.body);
				") (text . "") (css . "
				div { color:red; }
				") (text . "") (html . "<div>First</div>
				<div>Second</div>  
				<div>Third</div>
				
				<div>Fourth</div>") (text . "")) ((text . "") (text . "Turn a jQuery object into an array

") (text . "") (js . "
				var obj = $('li');
				var arr = $.makeArray(obj);
				") (text . "") (text . "(typeof obj === ʼobjectʼ && obj.jquery) === true; jQuery.isArray(arr)
=== true;

") (text . ""))))) jquery-doc-hash)

(push "$.grep" jquery-methods)

(puthash "$.grep" (quote (("name" . "$.grep") ("signatures" "$.grep" (("array" "The array to search through.

" nil nil) ("function(elementOfArray, indexInArray)" "The function to process each item against. The first argument to the
function is the item, and the second argument is the index. The
function should return a Boolean value. this will be the global window
object.
" nil nil) ("invert" "If \"invert\" is false, or not provided, then the function returns an
array consisting of all elements for which \"callback\" returns true. If
\"invert\" is true, then the function returns an array consisting of all
elements for which \"callback\" returns false.
" "true" nil))) ("desc" (text . "Finds the elements of an array which satisfy a filter function. The
original array is not affected.

")) ("longdesc" (text . "The $.grep() method removes items from an array as necessary so that
all remaining items pass a provided test. The test is a function that
is passed an array item and the index of the item within the array.
Only if the test returns true will the item be in the result array.
") (text . "") (text . " The filter function will be passed two arguments: the current array
item and its index. The filter function must return ʼtrueʼ to include
the item in the result array.
") (text . "")) ("examples" ((text . "") (text . "Filters the original array of numbers leaving that are not 5 and have
an index greater than 4. Then it removes all 9s.

") (text . "") (js . "
				var arr = [ 1, 9, 3, 8, 6, 1, 5, 9, 4, 7, 3, 8, 6, 9, 1 ];
				$(\"div\").text(arr.join(\", \"));

				arr = jQuery.grep(arr, function(n, i){
				return (n != 5 && i > 4);
				});
				$(\"p\").text(arr.join(\", \"));

				arr = jQuery.grep(arr, function (a) { return a != 9; });
				$(\"span\").text(arr.join(\", \"));

				") (text . "") (css . "
				div { color:blue; }
				p { color:green; margin:0; }
				span { color:red; }
				") (text . "") (html . "<div></div>
				<p></p>
				<span></span>
				") (text . "")) ((text . "") (text . "Filter an array of numbers to include only numbers bigger then zero.

") (text . "") (js . "$.grep( [0,1,2], function(n,i){
				return n > 0;
				});") (text . "") (text . "[1, 2]

") (text . "")) ((text . "") (text . "Filter an array of numbers to include numbers that are not bigger than
zero.

") (text . "") (js . "$.grep( [0,1,2], function(n,i){
				return n > 0;
				},true);") (text . "") (text . "[0]

") (text . ""))))) jquery-doc-hash)

(push "$.extend" jquery-methods)

(puthash "$.extend" (quote (("name" . "$.extend") ("signatures" "$.extend" (("target" "An object that will receive the new properties if additional objects
are passed in or that will extend the jQuery namespace if it is the
sole argument.
" nil nil) ("object1" "An object containing additional properties to merge in.

" "true" nil) ("objectN" "Additional objects containing properties to merge in.

" "true" nil)) (("deep" "If true, the merge becomes recursive (aka. deep copy).

" "true" nil) ("target" "The object to extend. It will receive the new properties.

" nil nil) ("object1" "An object containing additional properties to merge in.

" nil nil) ("objectN" "Additional objects containing properties to merge in.

" "true" nil))) ("desc" (text . "Merge the contents of two or more objects together into the first
object.

")) ("longdesc" (text . "When we supply two or more objects to $.extend(), properties from all
of the objects are added to the target object.

") (text . "") (text . "If only one argument is supplied to $.extend(), this means the target
argument was omitted. In this case, the jQuery object itself is assumed
to be the target. By doing this, we can add new functions to the jQuery
namespace. This can be useful for plugin authors wishing to add new
methods to JQuery.
") (text . "") (text . "Keep in mind that the target object (first argument) will be modified,
and will also be returned from $.extend(). If, however, we want to
preserve both of the original objects, we can do so by passing an empty
object as the target:
") (text . "") (js . "var object = $.extend({}, object1, object2);") (text . "") (text . "The merge performed by $.extend() is not recursive by default; if a
property of the first object is itself an object or array, it will be
completely overwritten by a property with the same key in the second
object. The values are not merged. This can be seen in the example
below by examining the value of banana. However, by passing true for
the first function argument, objects will be recursively merged.
") (text . "") (text . "Undefined properties are not copied. However, properties inherited from
the objectʼs prototype will be copied over.

") (text . "")) ("examples" ((text . "") (text . "Merge two objects, modifying the first.

") (text . "") (js . "
				var object1 = {
				apple: 0,
				banana: {weight: 52, price: 100},
				cherry: 97
				};
				var object2 = {
				banana: {price: 200},
				durian: 100
				};

				$.extend(object1, object2);
				") (text . "") (text . "object1 === {apple: 0, banana: {price: 200}, cherry: 97, durian: 100}

") (text . "")) ((text . "") (text . "Merge two objects recursively, modifying the first.

") (text . "") (js . "
				var object1 = {
				apple: 0,
				banana: {weight: 52, price: 100},
				cherry: 97
				};
				var object2 = {
				banana: {price: 200},
				lime: 100
				};

				$.extend(true, object1, object2);
				") (text . "") (text . "object1 === {apple: 0, banana: {weight: 52, price: 200}, cherry: 97,
lime: 100}

") (text . "")) ((text . "") (text . "Merge settings and options, modifying settings.

") (text . "") (js . "var settings = { validate: false, limit: 5, name: \"foo\" };
				var options = { validate: true, name: \"bar\" };
				jQuery.extend(settings, options);") (text . "") (text . "settings == { validate: true, limit: 5, name: \"bar\" }

") (text . "")) ((text . "") (text . "Merge defaults and options, without modifying the defaults. This is a
common plugin development pattern.

") (text . "") (js . "var empty = {}
				var defaults = { validate: false, limit: 5, name: \"foo\" };
				var options = { validate: true, name: \"bar\" };
				var settings = $.extend(empty, defaults, options);") (text . "") (text . "settings == { validate: true, limit: 5, name: \"bar\" } empty == {
validate: true, limit: 5, name: \"bar\" }

") (text . ""))))) jquery-doc-hash)

(push "$.each" jquery-methods)

(puthash "$.each" (quote (("name" . "$.each") ("signatures" "$.each" (("collection" "The object or array to iterate over.

" nil nil) ("callback(indexInArray, valueOfElement)" "The function that will be executed on every object.

" nil nil))) ("desc" (text . "A generic iterator function, which can be used to seamlessly iterate
over both objects and arrays. Arrays and array-like objects with a
length property (such as a functionʼs arguments object) are iterated by
numeric index, from 0 to length-1. Other objects are iterated via their
named properties.
")) ("longdesc" (text . "") (text . "The $.each() function is not the same as .each(), which is used to
iterate, exclusively, over a jQuery object. The $.each() function can
be used to iterate over any collection, whether it is a map (JavaScript
object) or an array. In the case of an array, the callback is passed an
array index and a corresponding array value each time. (The value can
also be accessed through the this keyword, but Javascript will always
wrap the this value as an Object even if it is a simple string or
number value.) The method returns its first argument, the object that
was iterated.
") (text . "") (js . "$.each([52, 97], function(index, value) { 
				alert(index + ': ' + value); 
				});
				") (text . "") (text . "This produces two messages:


") (text . "") (text . "0: 52
1: 97

") (text . "") (text . "If a map is used as the collection, the callback is passed a key-value
pair each time:

") (text . "") (js . "var map = { 
				'flammable': 'inflammable', 
				'duh': 'no duh' 
				}; 
				$.each(map, function(key, value) { 
				alert(key + ': ' + value); 
				});") (text . "") (text . "Once again, this produces two messages:


") (text . "") (text . "flammable: inflammable
duh: no duh

") (text . "") (text . "We can break the $.each() loop at a particular iteration by making the
callback function return false. Returning non-false is the same as a
continue statement in a for loop; it will skip immediately to the next
iteration.
")) ("examples" ((text . "") (text . "Iterates through the array displaying each number as both a word and
numeral

") (text . "") (js . "
				var arr = [ \"one\", \"two\", \"three\", \"four\", \"five\" ];
				var obj = { one:1, two:2, three:3, four:4, five:5 };
				
				jQuery.each(arr, function() {
				$(\"#\" + this).text(\"Mine is \" + this + \".\");
				return (this != \"three\"); // will stop running after \"three\"
				});
				
				jQuery.each(obj, function(i, val) {
				$(\"#\" + i).append(document.createTextNode(\" - \" + val));
				});
				") (text . "") (css . "
				div { color:blue; }
				div#five { color:red; }
				") (text . "") (html . "
				<div id=\"one\"></div>
				<div id=\"two\"></div>
				<div id=\"three\"></div>
				<div id=\"four\"></div>
				<div id=\"five\"></div>") (text . "")) ((text . "") (text . "Iterates over items in an array, accessing both the current item and
its index.

") (text . "") (js . "$.each( ['a','b','c'], function(i, l){
				alert( \"Index #\" + i + \": \" + l );
				});") (text . "")) ((text . "") (text . "Iterates over the properties in an object, accessing both the current
item and its key.

") (text . "") (js . "$.each( { name: \"John\", lang: \"JS\" }, function(k, v){
				alert( \"Key: \" + k + \", Value: \" + v );
				});") (text . ""))))) jquery-doc-hash)

(push "scrollTop" jquery-methods)

(puthash "scrollTop" (quote (("name" . "scrollTop") ("signatures" "scrollTop" nil) ("desc" (text . "Get the current vertical position of the scroll bar for the first
element in the set of matched elements.

")) ("longdesc" (text . "The vertical scroll position is the same as the number of pixels that
are hidden from view above the scrollable area. If the scroll bar is at
the very top, or if the element is not scrollable, this number will be
0.
")) ("examples" ((text . "") (text . "Get the scrollTop of a paragraph.

") (text . "") (js . "var p = $(\"p:first\");
				$(\"p:last\").text( \"scrollTop:\" + p.scrollTop() );
				
				") (text . "") (css . "
				p { margin:10px;padding:5px;border:2px solid #666; }
				") (text . "") (html . "<p>Hello</p><p></p>") (text . ""))))) jquery-doc-hash)

(push "scrollTop" jquery-methods)

(puthash "scrollTop" (quote (("name" . "scrollTop") ("signatures" "scrollTop" (("value" "An integer indicating the new position to set the scroll bar to.

" nil nil))) ("desc" (text . "Set the current vertical position of the scroll bar for each of the set
of matched elements.

")) ("longdesc" (text . "The vertical scroll position is the same as the number of pixels that
are hidden from view above the scrollable area. Setting the scrollTop
positions the vertical scroll of each matched element.
")) ("examples" ((text . "") (text . "Set the scrollTop of a div.

") (text . "") (js . "$(\"div.demo\").scrollTop(300);
				") (text . "") (css . "
				div.demo {
				background:#CCCCCC none repeat scroll 0 0;
				border:3px solid #666666;
				margin:5px;
				padding:5px;
				position:relative;
				width:200px;
				height:100px;
				overflow:auto;
				}
				p { margin:10px;padding:5px;border:2px solid #666;width:1000px;height:1000px; }
				") (text . "") (html . "<div class=\"demo\"><h1>lalala</h1><p>Hello</p></div>") (text . ""))))) jquery-doc-hash)

(push "position" jquery-methods)

(puthash "position" (quote (("name" . "position") ("signatures" "position" nil) ("desc" (text . "Get the current coordinates of the first element in the set of matched
elements, relative to the offset parent.

")) ("longdesc" (text . "The .position() method allows us to retrieve the current position of an
element relative to the offset parent. Contrast this with .offset() ,
which retrieves the current position relative to the document. When
positioning a new element near another one and within the same
containing DOM element, .position() is the more useful.
") (text . "") (text . "Returns an object containing the properties top and left.


")) ("examples" ((text . "") (text . "Access the position of the second paragraph:

") (text . "") (js . "
				var p = $(\"p:first\");
				var position = p.position();
				$(\"p:last\").text( \"left: \" + position.left + \", top: \" + position.top );
				") (text . "") (css . "
				
				div { padding: 15px;}
				p { margin-left:10px; }
				") (text . "") (html . "
				<div>
				<p>Hello</p>
				</div>
				<p></p>
				") (text . ""))))) jquery-doc-hash)

(push "offset" jquery-methods)

(puthash "offset" (quote (("name" . "offset") ("signatures" "offset" nil) ("desc" (text . "Get the current coordinates of the first element in the set of matched
elements, relative to the document.

")) ("longdesc" (text . "The .offset() method allows us to retrieve the current position of an
element relative to the document. Contrast this with .position(), which
retrieves the current position relative to the offset parent. When
positioning a new element on top of an existing one for global
manipulation (in particular, for implementing drag-and-drop), .offset()
is the more useful.
") (text . "") (text . ".offset() returns an object containing the properties top and left.


") (text . "") (text . "  Note: jQuery does not support getting the offset coordinates of
  hidden elements or accounting for borders, margins, or padding set
  on the body element.
") (text . "")) ("examples" ((text . "") (text . "Access the offset of the second paragraph:

") (text . "") (js . "var p = $(\"p:last\");
				var offset = p.offset();
				p.html( \"left: \" + offset.left + \", top: \" + offset.top );") (text . "") (css . "
				p { margin-left:10px; }
				") (text . "") (html . "<p>Hello</p><p>2nd Paragraph</p>") (text . "")) ((text . "") (text . "Click to see the offset.

") (text . "") (js . "
				$(\"*\", document.body).click(function (e) {
				var offset = $(this).offset();
				e.stopPropagation();
				$(\"#result\").text(this.tagName + \" coords ( \" + offset.left + \", \" +
				offset.top + \" )\");
				});

				") (text . "") (css . "
				p { margin-left:10px; color:blue; width:200px; 
				cursor:pointer; }
				span { color:red; cursor:pointer; }
				div.abs { width:50px; height:50px; position:absolute;
				left:220px; top:35px; background-color:green; 
				cursor:pointer; }
				") (text . "") (html . "<div id=\"result\">Click an element.</div>
				<p>
				This is the best way to <span>find</span> an offset.
				</p>

				<div class=\"abs\">
				</div>
				") (text . ""))))) jquery-doc-hash)

(push "offset" jquery-methods)

(puthash "offset" (quote (("name" . "offset") ("signatures" "offset" (("coordinates" "An object containing the properties top and left, which are integers
indicating the new top and left coordinates for the elements.

" nil nil)) (("function(index, coords)" "A function to return the coordinates to set. Receives the index of the
element in the collection as the first argument and the current
coordinates as the second argument. The function should return an
object with the new top and left properties.
" nil nil))) ("desc" (text . "Set the current coordinates of every element in the set of matched
elements, relative to the document.

")) ("longdesc" (text . "The .offset() setter method allows us to reposition an element. The
elementʼs position is specified relative to the document. If the
elementʼs position style property is currently static, it will be set
to relative to allow for this repositioning.
")) ("examples" ((text . "") (text . "Set the offset of the second paragraph:

") (text . "") (js . "$(\"p:last\").offset({ top: 10, left: 30 });") (text . "") (css . "p { margin-left:10px; } ") (text . "") (html . "<p>Hello</p><p>2nd Paragraph</p>") (text . ""))))) jquery-doc-hash)

(push "css" jquery-methods)

(puthash "css" (quote (("name" . "css") ("signatures" "css" (("propertyName" "A CSS property.

" nil nil))) ("desc" (text . "Get the value of a style property for the first element in the set of
matched elements.

")) ("longdesc" (text . "The .css() method is a convenient way to get a style property from the
first matched element, especially in light of the different ways
browsers access most of those properties (the getComputedStyle() method
in standards-based browsers versus the currentStyle and runtimeStyle
properties in Internet Explorer) and the different terms browsers use
for certain properties. For example, Internet Explorerʼs DOM
implementation refers to the float property as styleFloat, while W3C
standards-compliant browsers refer to it as cssFloat. The .css() method
accounts for such differences, producing the same result no matter
which term we use. For example, an element that is floated left will
return the string left for each of the following three lines:
") (text . "") (text . " 1. $(ʼdiv.leftʼ).css(ʼfloatʼ);
 2. $(ʼdiv.leftʼ).css(ʼcssFloatʼ);
 3. $(ʼdiv.leftʼ).css(ʼstyleFloatʼ);
") (text . "") (text . "Also, jQuery can equally interpret the CSS and DOM formatting of
multiple-word properties. For example, jQuery understands and returns
the correct value for both .css(ʼbackground-colorʼ) and
.css(ʼbackgroundColorʼ).
") (text . "") (text . "Shorthand CSS properties (e.g. margin, background, border) are not
supported. For example, if you want to retrieve the rendered margin,
use: $(elem).css(ʼmarginTopʼ) and $(elem).css(ʼmarginRightʼ), and so
on.
")) ("examples" ((text . "") (text . "To access the background color of a clicked div.

") (text . "") (js . "
				$(\"div\").click(function () {
				var color = $(this).css(\"background-color\");
				$(\"#result\").html(\"That div is <span style='color:\" +
				color + \";'>\" + color + \"</span>.\");
				});
				
				") (text . "") (css . "
				div { width:60px; height:60px; margin:5px; float:left; }
				") (text . "") (html . "
				<span id=\"result\">&nbsp;</span>
				<div style=\"background-color:blue;\"></div>
				<div style=\"background-color:rgb(15,99,30);\"></div>
				
				<div style=\"background-color:#123456;\"></div>
				<div style=\"background-color:#f11;\"></div>") (text . ""))))) jquery-doc-hash)

(push "css" jquery-methods)

(puthash "css" (quote (("name" . "css") ("signatures" "css" (("propertyName" "A CSS property name.

" nil nil) ("value" "A value to set for the property.

" nil nil)) (("propertyName" "A CSS property name.

" nil nil) ("function(index, value)" "A function returning the value to set. Receives the index position of
the element in the set and the old value as arguments.

" nil nil)) (("map" "A map of property-value pairs to set.

" nil nil))) ("desc" (text . "Set one or more CSS properties for the set of matched elements.

")) ("longdesc" (text . "As with the .prop() method, the .css() method makes setting properties
of elements quick and easy. This method can take either a property name
and value as separate parameters, or a single map of key-value pairs
(JavaScript object notation).
") (text . "") (text . "Also, jQuery can equally interpret the CSS and DOM formatting of
multiple-word properties. For example, jQuery understands and returns
the correct value for both .css({ʼbackground-colorʼ: ʼ#ffeʼ,
ʼborder-leftʼ: ʼ5px solid #cccʼ}) and .css({backgroundColor: ʼ#ffeʼ,
borderLeft: ʼ5px solid #cccʼ}). Notice that with the DOM notation,
quotation marks around the property names are optional, but with CSS
notation theyʼre required due to the hyphen in the name.
") (text . "") (text . "When using .css() as a setter, jQuery modifies the elementʼs style
property. For example, $(ʼ#mydivʼ).css(ʼcolorʼ, ʼgreenʼ) is equivalent
to document.getElementById(ʼmydivʼ).style.color = ʼgreenʼ. Setting the
value of a style property to an empty string â e.g.
$(ʼ#mydivʼ).css(ʼcolorʼ, ʼʼ) â removes that property from an element if
it has already been directly applied, whether in the HTML style
attribute, through jQueryʼs .css() method, or through direct DOM
manipulation of the style property. It does not, however, remove a
style that has been applied with a CSS rule in a stylesheet or <style>
element.
") (text . "") (text . "As of jQuery 1.6, .css() accepts relative values similar to .animate().
Relative values are a string starting with += or -= to increment or
decrement the current value. For example, if an elementʼs padding-left
was 10px, .css( \"padding-left\", \"+=15\" ) would result in a total
padding-left of 25px.
") (text . "") (text . "As of jQuery 1.4, .css() allows us to pass a function as the property
value:

") (text . "") (js . "$('div.example').css('width', function(index) {
			      return index * 50;
			      });") (text . "") (text . "This example sets the widths of the matched elements to incrementally
larger values.

")) ("examples" ((text . "") (text . "To change the color of any paragraph to red on mouseover event.

") (text . "") (js . "
				$(\"p\").mouseover(function () {
				$(this).css(\"color\",\"red\");
				});
				") (text . "") (css . "
				p { color:blue; width:200px; font-size:14px; }
				") (text . "") (html . "
				<p>Just roll the mouse over me.</p>
				
				<p>Or me to see a color change.</p>
				") (text . "")) ((text . "") (text . "Increase the width of #box by 200 pixels

") (text . "") (js . "
				$(\"#box\").one( \"click\", function () {
				$( this ).css( \"width\",\"+=200\" );
				});
				") (text . "") (css . "
				#box { background: black; color: snow; width:100px; padding:10px; }
				") (text . "") (html . "
				<div id=\"box\">Click me to grow</div>
				") (text . "")) ((text . "") (text . "To highlight a clicked word in the paragraph.

") (text . "") (js . "
				var words = $(\"p:first\").text().split(\" \");
				var text = words.join(\"</span> <span>\");
				$(\"p:first\").html(\"<span>\" + text + \"</span>\");
				$(\"span\").click(function () {
				$(this).css(\"background-color\",\"yellow\");
				});
				
				") (text . "") (css . "
				p { color:blue; font-weight:bold; cursor:pointer; }
				") (text . "") (html . "
				<p>
				Once upon a time there was a man
				who lived in a pizza parlor. This
				man just loved pizza and ate it all 
				the time.  He went on to be the
				happiest man in the world.  The end.
				</p>") (text . "")) ((text . "") (text . "To set the color of all paragraphs to red and background to blue:

") (text . "") (js . "
				$(\"p\").hover(function () {
				$(this).css({'background-color' : 'yellow', 'font-weight' : 'bolder'});
				}, function () {
				var cssObj = {
				'background-color' : '#ddd',
				'font-weight' : '',
				'color' : 'rgb(0,40,244)'
				}
				$(this).css(cssObj);
				});
				") (text . "") (css . "
				p { color:green; }
				") (text . "") (html . "
				<p>Move the mouse over a paragraph.</p>
				<p>Like this one or the one above.</p>
				") (text . "")) ((text . "") (text . "Increase the size of a div when you click it:

") (text . "") (js . "
				$(\"div\").click(function() {
				$(this).css({
				width: function(index, value) {
				return parseFloat(value) * 1.2;
				}, 
				height: function(index, value) {
				return parseFloat(value) * 1.2;
				}
				
				});
				});
				") (text . "") (css . "
				div { width: 20px; height: 15px; background-color: #f33; }
				") (text . "") (html . "
				<div>click</div>
				<div>click</div>
				") (text . ""))))) jquery-doc-hash)

(push "unwrap" jquery-methods)

(puthash "unwrap" (quote (("name" . "unwrap") ("signatures" "unwrap" nil) ("desc" (text . "Remove the parents of the set of matched elements from the DOM, leaving
the matched elements in their place.

")) ("longdesc" (text . "The .unwrap() method removes the elementʼs parent. This is effectively
the inverse of the .wrap() method. The matched elements (and their
siblings, if any) replace their parents within the DOM structure.
")) ("examples" ((text . "") (text . "Wrap/unwrap a div around each of the paragraphs.

") (text . "") (js . "
				$(\"button\").toggle(function(){
				$(\"p\").wrap(\"<div></div>\");
				}, function(){
				$(\"p\").unwrap();
				});") (text . "") (css . "
				div { border: 2px solid blue; }
				p { background:yellow; margin:4px; }
				") (text . "") (html . "<button>wrap/unwrap</button>
				<p>Hello</p>
				<p>cruel</p>
				<p>World</p>") (text . ""))))) jquery-doc-hash)

(push "detach" jquery-methods)

(puthash "detach" (quote (("name" . "detach") ("signatures" "detach" (("selector" "A selector expression that filters the set of matched elements to be
removed.

" "true" nil))) ("desc" (text . "Remove the set of matched elements from the DOM.

")) ("longdesc" (text . "The .detach() method is the same as .remove() , except that .detach()
keeps all jQuery data associated with the removed elements. This method
is useful when removed elements are to be reinserted into the DOM at a
later time.
")) ("examples" ((text . "") (text . "Detach all paragraphs from the DOM

") (text . "") (js . "
				$(\"p\").click(function(){
				$(this).toggleClass(\"off\");
				});
				var p;
				$(\"button\").click(function(){
				if ( p ) {
				p.appendTo(\"body\");
				p = null;
				} else {
				p = $(\"p\").detach();
				}
				});") (text . "") (css . "p { background:yellow; margin:6px 0; } p.off { background: black; }") (text . "") (html . "<p>Hello</p> 
				how are 
				<p>you?</p>
				<button>Attach/detach paragraphs</button>") (text . ""))))) jquery-doc-hash)

(push "clone" jquery-methods)

(puthash "clone" (quote (("name" . "clone") ("signatures" "clone" (("withDataAndEvents" "A Boolean indicating whether event handlers should be copied along with
the elements. As of jQuery 1.4, element data will be copied as well.

" "true" nil)) (("withDataAndEvents" "A Boolean indicating whether event handlers and data should be copied
along with the elements. The default value is false. *For 1.5.0 the
default value is incorrectly true. This will be changed back to false
in 1.5.1 and up.
" "true" nil) ("deepWithDataAndEvents" "A Boolean indicating whether event handlers and data for all children
of the cloned element should be copied. By default its value matches
the first argumentʼs value (which defaults to false).
" "true" nil))) ("desc" (text . "Create a deep copy of the set of matched elements.

")) ("longdesc" (text . "The .clone() method performs a deep copy of the set of matched
elements, meaning that it copies the matched elements as well as all of
their descendant elements and text nodes. When used in conjunction with
one of the insertion methods, .clone() is a convenient way to duplicate
elements on a page. Consider the following HTML:
") (text . "") (js . "<div class=\"container\">
			      <div class=\"hello\">Hello</div>
			      <div class=\"goodbye\">Goodbye</div>
			      </div>") (text . "") (text . "As shown in the discussion for .append() , normally when an element is
inserted somewhere in the DOM, it is moved from its old location. So,
given the code:
") (text . "") (js . "$('.hello').appendTo('.goodbye');") (text . "") (text . "The resulting DOM structure would be:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"goodbye\">
			      Goodbye
			      <div class=\"hello\">Hello</div>
			      </div>
			      </div>") (text . "") (text . "To prevent this and instead create a copy of the element, you could
write the following:

") (text . "") (js . "$('.hello').clone().appendTo('.goodbye');") (text . "") (text . "This would produce:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"hello\">Hello</div>
			      <div class=\"goodbye\">
			      Goodbye
			      <div class=\"hello\">Hello</div>
			      </div>
			      </div>") (text . "") (text . "  Note that when using the .clone() method, you can modify the cloned
  elements or their contents before (re-)inserting them into the
  document.
") (text . "") (text . "Normally, any event handlers bound to the original element are not
copied to the clone. The optional withDataAndEvents parameter allows us
to change this behavior, and to instead make copies of all of the event
handlers as well, bound to the new copy of the element. As of jQuery
1.4, all element data (attached by the .data() method) is also copied
to the new copy.
") (text . "") (text . "However, objects and arrays within element data are not copied and will
continue to be shared between the cloned element and the original
element. To deep copy all data, copy each one manually:
") (text . "") (js . "var $elem = $('#elem').data( \"arr\": [ 1 ] ), // Original element with attached data
			      $clone = $elem.clone( true )
			      .data( \"arr\", $.extend( [], $elem.data(\"arr\") ) ); // Deep copy to prevent data sharing
			      ") (text . "") (text . "As of jQuery 1.5, withDataAndEvents can be optionally enhanced with
deepWithDataAndEvents to copy the events and data for all children of
the cloned element.
") (text . "")) ("examples" ((text . "") (text . "Clones all b elements (and selects the clones) and prepends them to all
paragraphs.

") (text . "") (js . "
				$(\"b\").clone().prependTo(\"p\");
				") (text . "") (html . "
				<b>Hello</b><p>, how are you?</p>
				") (text . ""))))) jquery-doc-hash)

(push "remove" jquery-methods)

(puthash "remove" (quote (("name" . "remove") ("signatures" "remove" (("selector" "A selector expression that filters the set of matched elements to be
removed.

" "true" nil))) ("desc" (text . "Remove the set of matched elements from the DOM.

")) ("longdesc" (text . "Similar to .empty() , the .remove() method takes elements out of the
DOM. Use .remove() when you want to remove the element itself, as well
as everything inside it. In addition to the elements themselves, all
bound events and jQuery data associated with the elements are removed.
To remove the elements without removing data and events, use .detach()
instead.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"hello\">Hello</div>
			      <div class=\"goodbye\">Goodbye</div>
			      </div>") (text . "") (text . "We can target any element for removal:


") (text . "") (js . "$('.hello').remove();") (text . "") (text . "This will result in a DOM structure with the <div> element deleted:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"goodbye\">Goodbye</div>
			      </div>") (text . "") (text . "If we had any number of nested elements inside <div class=\"hello\">,
they would be removed, too. Other jQuery constructs such as data or
event handlers are erased as well.
") (text . "") (text . "We can also include a selector as an optional parameter. For example,
we could rewrite the previous DOM removal code as follows:

") (text . "") (js . "$('div').remove('.hello');") (text . "") (text . "This would result in the same DOM structure:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"goodbye\">Goodbye</div>
			      </div>")) ("examples" ((text . "") (text . "Removes all paragraphs from the DOM

") (text . "") (js . "
				$(\"button\").click(function () {
				$(\"p\").remove();
				});
				
				") (text . "") (css . "p { background:yellow; margin:6px 0; }") (text . "") (html . "<p>Hello</p> 
				how are 
				<p>you?</p>
				<button>Call remove() on paragraphs</button>") (text . "")) ((text . "") (text . "Removes all paragraphs that contain \"Hello\" from the DOM. Analogous to
doing $(\"p\").filter(\":contains(ʼHelloʼ)\").remove().

") (text . "") (js . "
				
				$(\"button\").click(function () {
				$(\"p\").remove(\":contains('Hello')\");
				});
				
				") (text . "") (css . "p { background:yellow; margin:6px 0; }") (text . "") (html . "<p class=\"hello\">Hello</p>
				how are 
				<p>you?</p>
				
				<button>Call remove(\":contains('Hello')\") on paragraphs</button>") (text . ""))))) jquery-doc-hash)

(push "empty" jquery-methods)

(puthash "empty" (quote (("name" . "empty") ("signatures" "empty" nil) ("desc" (text . "Remove all child nodes of the set of matched elements from the DOM.

")) ("longdesc" (text . "This method removes not only child (and other descendant) elements, but
also any text within the set of matched elements. This is because,
according to the DOM specification, any string of text within an
element is considered a child node of that element. Consider the
following HTML:
") (text . "") (js . "<div class=\"container\">
			      <div class=\"hello\">Hello</div>
			      <div class=\"goodbye\">Goodbye</div>
			      </div>") (text . "") (text . "We can target any element for removal:


") (text . "") (js . "$('.hello').empty();") (text . "") (text . "This will result in a DOM structure with the Hello text deleted:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"hello\"></div>
			      <div class=\"goodbye\">Goodbye</div>
			      </div>") (text . "") (text . "If we had any number of nested elements inside <div class=\"hello\">,
they would be removed, too.

") (text . "") (text . "To avoid memory leaks, jQuery removes other constructs such as data and
event handlers from the child elements before removing the elements
themselves.
") (text . "")) ("examples" ((text . "") (text . "Removes all child nodes (including text nodes) from all paragraphs

") (text . "") (js . "
				$(\"button\").click(function () {
				$(\"p\").empty();
				});
				") (text . "") (css . "
				p { background:yellow; }
				") (text . "") (html . "<p>
				Hello, <span>Person</span> <a href=\"javascript:;\">and person</a>
				</p>

				<button>Call empty() on above paragraph</button>") (text . ""))))) jquery-doc-hash)

(push "replaceAll" jquery-methods)

(puthash "replaceAll" (quote (("name" . "replaceAll") ("signatures" "replaceAll" (("target" "A selector expression indicating which element(s) to replace.

" nil nil))) ("desc" (text . "Replace each target element with the set of matched elements.

")) ("longdesc" (text . "The .replaceAll() method is corollary to .replaceWith() , but with the
source and target reversed. Consider this DOM structure:

") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner first\">Hello</div>
			      <div class=\"inner second\">And</div>
			      <div class=\"inner third\">Goodbye</div>
			      </div>") (text . "") (text . "We can create an element, then replace other elements with it:


") (text . "") (js . "$('<h2>New heading</h2>').replaceAll('.inner');") (text . "") (text . "This causes all of them to be replaced:


") (text . "") (js . "<div class=\"container\">
			      <h2>New heading</h2>
			      <h2>New heading</h2>
			      <h2>New heading</h2>
			      </div>") (text . "") (text . "Or, we could select an element to use as the replacement:


") (text . "") (js . "$('.first').replaceAll('.third');") (text . "") (text . "This results in the DOM structure:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner second\">And</div>
			      <div class=\"inner first\">Hello</div>
			      </div>") (text . "") (text . "From this example, we can see that the selected element replaces the
target by being moved from its old location, not by being cloned.

")) ("examples" ((text . "") (text . "Replace all the paragraphs with bold words.

") (text . "") (js . "$(\"<b>Paragraph. </b>\").replaceAll(\"p\"); // check replaceWith() examples") (text . "") (html . "<p>Hello</p>
				<p>cruel</p>
				<p>World</p>") (text . ""))))) jquery-doc-hash)

(push "replaceWith" jquery-methods)

(puthash "replaceWith" (quote (("name" . "replaceWith") ("signatures" "replaceWith" (("newContent" "The content to insert. May be an HTML string, DOM element, or jQuery
object.

" nil nil)) (("function" "A function that returns content with which to replace the set of
matched elements.

" nil nil))) ("desc" (text . "Replace each element in the set of matched elements with the provided
new content.

")) ("longdesc" (text . "The .replaceWith() method removes content from the DOM and inserts new
content in its place with a single call. Consider this DOM structure:

") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner first\">Hello</div>
			      <div class=\"inner second\">And</div>
			      <div class=\"inner third\">Goodbye</div>
			      </div>") (text . "") (text . "The second inner <div> could be replaced with the specified HTML:


") (text . "") (js . "$('div.second').replaceWith('<h2>New heading</h2>');") (text . "") (text . "This results in the structure:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner first\">Hello</div>
			      <h2>New heading</h2>
			      <div class=\"inner third\">Goodbye</div>
			      </div>") (text . "") (text . "All inner <div> elements could be targeted at once:


") (text . "") (js . "$('div.inner').replaceWith('<h2>New heading</h2>');") (text . "") (text . "This causes all of them to be replaced:


") (text . "") (js . "<div class=\"container\">
			      <h2>New heading</h2>
			      <h2>New heading</h2>
			      <h2>New heading</h2>
			      </div>") (text . "") (text . "An element could also be selected as the replacement:


") (text . "") (js . "$('div.third').replaceWith($('.first'));") (text . "") (text . "This results in the DOM structure:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner second\">And</div>
			      <div class=\"inner first\">Hello</div>
			      </div>") (text . "") (text . "This example demonstrates that the selected element replaces the target
by being moved from its old location, not by being cloned.

") (text . "") (text . "The .replaceWith() method, like most jQuery methods, returns the jQuery
object so that other methods can be chained onto it. However, it must
be noted that the original jQuery object is returned. This object
refers to the element that has been removed from the DOM, not the new
element that has replaced it.
") (text . "") (text . "As of jQuery 1.4, .replaceWith() can also work on disconnected DOM
nodes. For example, with the following code, .replaceWith() returns a
jQuery set containing only a paragraph.:
") (text . "") (js . "$(\"<div/>\").replaceWith(\"<p></p>\");") (text . "") (text . "The .replaceWith() method can also take a function as its argument:


") (text . "") (js . "$('div.container').replaceWith(function() {
			      return $(this).contents();
			      });") (text . "") (text . "This results in <div class=\"container\"> being replaced by its three
child <div>s. The return value of the function may be an HTML string,
DOM element, or jQuery object.
") (text . "")) ("examples" ((text . "") (text . "On click, replace the button with a div containing the same word.

") (text . "") (js . "
				$(\"button\").click(function () {
				$(this).replaceWith( \"<div>\" + $(this).text() + \"</div>\" );
				});
				") (text . "") (css . "
				button { display:block; margin:3px; color:red; width:200px; }
				div { color:red; border:2px solid blue; width:200px;
				margin:3px; text-align:center; }
				") (text . "") (html . "
				<button>First</button>
				<button>Second</button>
				<button>Third</button>
				") (text . "")) ((text . "") (text . "Replace all paragraphs with bold words.

") (text . "") (js . "
				$(\"p\").replaceWith( \"<b>Paragraph. </b>\" );
				") (text . "") (html . "
				<p>Hello</p>
				<p>cruel</p>
				<p>World</p>
				") (text . "")) ((text . "") (text . "Replace all paragraphs with empty div elements.

") (text . "") (js . "
				$(\"p\").replaceWith( document.createElement(\"div\") );") (text . "") (css . "
				div { border:2px solid blue; margin:3px; }
				") (text . "") (html . "
				<p>Hello</p>
				<p>cruel</p>
				<p>World</p>
				") (text . "")) ((text . "") (text . "On click, replace each paragraph with a div that is already in the DOM
and selected with the $() function. Notice it doesnʼt clone the object
but rather moves it to replace the paragraph.
") (text . "") (js . "
				$(\"p\").click(function () {
				$(this).replaceWith( $(\"div\") );
				});
				") (text . "") (css . "
				div { border:2px solid blue; color:red; margin:3px; }
				p { border:2px solid red; color:blue; margin:3px; cursor:pointer; }
				") (text . "") (html . "
				<p>Hello</p>
				<p>cruel</p>
				<p>World</p>

				<div>Replaced!</div>
				") (text . "")) ((text . "") (text . "On button click, replace the containing div with its child divs and
append the class name of the selected element to the paragraph.

") (text . "") (js . "
				$('button').bind(\"click\", function() {
				var $container = $(\"div.container\").replaceWith(function() {
				return $(this).contents();
				});

				$(\"p\").append( $container.attr(\"class\") );
				});
				") (text . "") (css . " 
				.container { background-color: #991; }
				.inner { color: #911; }
				") (text . "") (html . "
				<p>
				<button>Replace!</button>
				</p>
				<div class=\"container\">
				<div class=\"inner\">Scooby</div>
				<div class=\"inner\">Dooby</div>
				<div class=\"inner\">Doo</div>
				</div>
				") (text . ""))))) jquery-doc-hash)

(push "wrapInner" jquery-methods)

(puthash "wrapInner" (quote (("name" . "wrapInner") ("signatures" "wrapInner" (("wrappingElement" "An HTML snippet, selector expression, jQuery object, or DOM element
specifying the structure to wrap around the content of the matched
elements.
" nil nil)) (("wrappingFunction" "A callback function which generates a structure to wrap around the
content of the matched elements.

" nil nil))) ("desc" (text . "Wrap an HTML structure around the content of each element in the set of
matched elements.

")) ("longdesc" (text . "The .wrapInner() function can take any string or object that could be
passed to the $() factory function to specify a DOM structure. This
structure may be nested several levels deep, but should contain only
one inmost element. The structure will be wrapped around the content of
each of the elements in the set of matched elements.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "Using .wrapInner(), we can insert an HTML structure around the content
of each inner <div> elements like so:

") (text . "") (js . "$('.inner').wrapInner('<div class=\"new\" />');") (text . "") (text . "The new <div> element is created on the fly and added to the DOM. The
result is a new <div> wrapped around the content of each matched
element:
") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner\">
			      <div class=\"new\">Hello</div>
			      </div>
			      <div class=\"inner\">
			      <div class=\"new\">Goodbye</div>
			      </div>
			      </div>") (text . "") (text . "The second version of this method allows us to instead specify a
callback function. This callback function will be called once for every
matched element; it should return a DOM element, jQuery object, or HTML
snippet in which to wrap the content of the corresponding element. For
example:
") (text . "") (js . "$('.inner').wrapInner(function() {
			      return '<div class=\"' + this.nodeValue + '\" />';
			      });") (text . "") (text . "This will cause each <div> to have a class corresponding to the text it
wraps:

") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner\">
			      <div class=\"Hello\">Hello</div>
			      </div>
			      <div class=\"inner\">
			      <div class=\"Goodbye\">Goodbye</div>
			      </div>
			      </div>") (text . "") (text . "Note: When passing a selector string to the .wrapInner() function, the
expected input is well formed HTML with correctly closed tags. Examples
of valid input include:
") (text . "") (js . "
				$(elem).wrapInner(\"<div class='test' />\");
				$(elem).wrapInner(\"<div class='test'></div>\");
				$(elem).wrapInner(\"<div class=\\\"test\\\"></div>\");
			      ") (text . "")) ("examples" ((text . "") (text . "Selects all paragraphs and wraps a bold tag around each of its
contents.

") (text . "") (js . "$(\"p\").wrapInner(\"<b></b>\");") (text . "") (css . "p { background:#bbf; }") (text . "") (html . "<p>Hello</p>

				<p>cruel</p>
				<p>World</p>") (text . "")) ((text . "") (text . "Wraps a newly created tree of objects around the inside of the body.

") (text . "") (js . "$(\"body\").wrapInner(\"<div><div><p><em><b></b></em></p></div></div>\");") (text . "") (css . "

				div { border:2px green solid; margin:2px; padding:2px; }
				p { background:yellow; margin:2px; padding:2px; }
				") (text . "") (html . "Plain old text, or is it?") (text . "")) ((text . "") (text . "Selects all paragraphs and wraps a bold tag around each of its
contents.

") (text . "") (js . "$(\"p\").wrapInner(document.createElement(\"b\"));") (text . "") (css . "p { background:#9f9; }") (text . "") (html . "<p>Hello</p>

				<p>cruel</p>
				<p>World</p>") (text . "")) ((text . "") (text . "Selects all paragraphs and wraps a jQuery object around each of its
contents.

") (text . "") (js . "$(\"p\").wrapInner($(\"<span class='red'></span>\"));") (text . "") (css . "

				p { background:#9f9; }
				.red { color:red; }
				") (text . "") (html . "<p>Hello</p>
				<p>cruel</p>
				<p>World</p>") (text . ""))))) jquery-doc-hash)

(push "wrapAll" jquery-methods)

(puthash "wrapAll" (quote (("name" . "wrapAll") ("signatures" "wrapAll" (("wrappingElement" "An HTML snippet, selector expression, jQuery object, or DOM element
specifying the structure to wrap around the matched elements.

" nil nil))) ("desc" (text . "Wrap an HTML structure around all elements in the set of matched
elements.

")) ("longdesc" (text . "The .wrapAll() function can take any string or object that could be
passed to the $() function to specify a DOM structure. This structure
may be nested several levels deep, but should contain only one inmost
element. The structure will be wrapped around all of the elements in
the set of matched elements, as a single group.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "Using .wrapAll(), we can insert an HTML structure around the inner
<div> elements like so:

") (text . "") (js . "$('.inner').wrapAll('<div class=\"new\" />');") (text . "") (text . "The new <div> element is created on the fly and added to the DOM. The
result is a new <div> wrapped around all matched elements:

") (text . "") (js . "<div class=\"container\">
			      <div class=\"new\">
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>
			      </div>")) ("examples" ((text . "") (text . "Wrap a new div around all of the paragraphs.

") (text . "") (js . "$(\"p\").wrapAll(\"<div></div>\");") (text . "") (css . "
				
				div { border: 2px solid blue; }
				p { background:yellow; margin:4px; }
				") (text . "") (html . "<p>Hello</p>
				<p>cruel</p>
				<p>World</p>") (text . "")) ((text . "") (text . "Wraps a newly created tree of objects around the spans. Notice anything
in between the spans gets left out like the <strong> (red text) in this
example. Even the white space between spans is left out. Click View
Source to see the original html.
") (text . "") (js . "$(\"span\").wrapAll(\"<div><div><p><em><b></b></em></p></div></div>\");") (text . "") (css . "
				
				div { border:2px blue solid; margin:2px; padding:2px; }
				p { background:yellow; margin:2px; padding:2px; }
				strong { color:red; }
				") (text . "") (html . "<span>Span Text</span>
				<strong>What about me?</strong>
				<span>Another One</span>") (text . "")) ((text . "") (text . "Wrap a new div around all of the paragraphs.

") (text . "") (js . "$(\"p\").wrapAll(document.createElement(\"div\"));") (text . "") (css . "
				
				div { border: 2px solid blue; }
				p { background:yellow; margin:4px; }
				") (text . "") (html . "<p>Hello</p>
				<p>cruel</p>
				<p>World</p>") (text . "")) ((text . "") (text . "Wrap a jQuery object double depth div around all of the paragraphs.
Notice it doesnʼt move the object but just clones it to wrap around its
target.
") (text . "") (js . "$(\"p\").wrapAll($(\".doublediv\"));") (text . "") (css . "
				
				div { border: 2px solid blue; margin:2px; padding:2px; }
				.doublediv { border-color:red; }
				p { background:yellow; margin:4px; font-size:14px; }
				") (text . "") (html . "<p>Hello</p>
				<p>cruel</p>
				<p>World</p>
				<div class=\"doublediv\"><div></div></div>") (text . ""))))) jquery-doc-hash)

(push "wrap" jquery-methods)

(puthash "wrap" (quote (("name" . "wrap") ("signatures" "wrap" (("wrappingElement" "An HTML snippet, selector expression, jQuery object, or DOM element
specifying the structure to wrap around the matched elements.

" nil nil)) (("function(index)" "A callback function returning the HTML content or jQuery object to wrap
around the matched elements.

" nil nil))) ("desc" (text . "Wrap an HTML structure around each element in the set of matched
elements.

")) ("longdesc" (text . "The .wrap() function can take any string or object that could be passed
to the $() factory function to specify a DOM structure. This structure
may be nested several levels deep, but should contain only one inmost
element. The structure will be wrapped around each of the elements in
the set of matched elements. This method returns the original set of
elements for chaining purposes.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "Using .wrap(), we can insert an HTML structure around the inner <div>
elements like so:

") (text . "") (js . "$('.inner').wrap('<div class=\"new\" />');") (text . "") (text . "The new <div> element is created on the fly and added to the DOM. The
result is a new <div> wrapped around each matched element:

") (text . "") (js . "<div class=\"container\">
			      <div class=\"new\">
			      <div class=\"inner\">Hello</div>
			      </div>
			      <div class=\"new\">
			      <div class=\"inner\">Goodbye</div>
			      </div>
			      </div>") (text . "") (text . "The second version of this method allows us to instead specify a
callback function. This callback function will be called once for every
matched element; it should return a DOM element, jQuery object, or HTML
snippet in which to wrap the corresponding element. For example:
") (text . "") (js . "$('.inner').wrap(function() {
			      return '<div class=\"' + $(this).text() + '\" />';
			      });") (text . "") (text . "This will cause each <div> to have a class corresponding to the text it
wraps:

") (text . "") (js . "<div class=\"container\">
			      <div class=\"Hello\">
			      <div class=\"inner\">Hello</div>
			      </div>
			      <div class=\"Goodbye\">
			      <div class=\"inner\">Goodbye</div>
			      </div>
			      </div>") (text . "")) ("examples" ((text . "") (text . "Wrap a new div around all of the paragraphs.

") (text . "") (js . "$(\"p\").wrap(\"<div></div>\");") (text . "") (css . "

				div { border: 2px solid blue; }
				p { background:yellow; margin:4px; }
				") (text . "") (html . "<p>Hello</p>
				<p>cruel</p>
				<p>World</p>") (text . "")) ((text . "") (text . "Wraps a newly created tree of objects around the spans. Notice anything
in between the spans gets left out like the <strong> (red text) in this
example. Even the white space between spans is left out. Click View
Source to see the original html.
") (text . "") (js . "$(\"span\").wrap(\"<div><div><p><em><b></b></em></p></div></div>\");") (text . "") (css . "

				div { border:2px blue solid; margin:2px; padding:2px; }
				p { background:yellow; margin:2px; padding:2px; }
				strong { color:red; }
				") (text . "") (html . "<span>Span Text</span>
				<strong>What about me?</strong>
				<span>Another One</span>") (text . "")) ((text . "") (text . "Wrap a new div around all of the paragraphs.

") (text . "") (js . "$(\"p\").wrap(document.createElement(\"div\"));") (text . "") (css . "

				div { border: 2px solid blue; }
				p { background:yellow; margin:4px; }
				") (text . "") (html . "<p>Hello</p>
				<p>cruel</p>
				<p>World</p>") (text . "")) ((text . "") (text . "Wrap a jQuery object double depth div around all of the paragraphs.
Notice it doesnʼt move the object but just clones it to wrap around its
target.
") (text . "") (js . "$(\"p\").wrap($(\".doublediv\"));") (text . "") (css . "

				div { border: 2px solid blue; margin:2px; padding:2px; }
				.doublediv { border-color:red; }
				p { background:yellow; margin:4px; font-size:14px; }
				") (text . "") (html . "<p>Hello</p>
				<p>cruel</p>
				<p>World</p>
				<div class=\"doublediv\"><div></div></div>") (text . ""))))) jquery-doc-hash)

(push "insertBefore" jquery-methods)

(puthash "insertBefore" (quote (("name" . "insertBefore") ("signatures" "insertBefore" (("target" "A selector, element, HTML string, or jQuery object; the matched set of
elements will be inserted before the element(s) specified by this
parameter.
" nil nil))) ("desc" (text . "Insert every element in the set of matched elements before the target.

")) ("longdesc" (text . "The .before() and .insertBefore() methods perform the same task. The
major difference is in the syntax-specifically, in the placement of the
content and target. With .before(), the selector expression preceding
the method is the container before which the content is inserted. With
.insertBefore(), on the other hand, the content precedes the method,
either as a selector expression or as markup created on the fly, and it
is inserted before the target container.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<div class=\"container\">
			      <h2>Greetings</h2>
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "We can create content and insert it before several elements at once:


") (text . "") (js . "$('<p>Test</p>').insertBefore('.inner');") (text . "") (text . "Each inner <div> element gets this new content:


") (text . "") (js . "<div class=\"container\">
			      <h2>Greetings</h2>
			      <p>Test</p>
			      <div class=\"inner\">Hello</div>
			      <p>Test</p>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "We can also select an element on the page and insert it before another:


") (text . "") (js . "$('h2').insertBefore($('.container'));") (text . "") (text . "If an element selected this way is inserted elsewhere, it will be moved
before the target (not cloned):

") (text . "") (js . "<h2>Greetings</h2>
			      <div class=\"container\">
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "If there is more than one target element, however, cloned copies of the
inserted element will be created for each target after the first.

")) ("examples" ((text . "") (text . "Inserts all paragraphs before an element with id of \"foo\". Same as
$(\"#foo\").before(\"p\")

") (text . "") (js . "$(\"p\").insertBefore(\"#foo\"); // check before() examples") (text . "") (css . "#foo { background:yellow; }") (text . "") (html . "<div id=\"foo\">FOO!</div><p>I would like to say: </p>") (text . ""))))) jquery-doc-hash)

(push "before" jquery-methods)

(puthash "before" (quote (("name" . "before") ("signatures" "before" (("content" "HTML string, DOM element, or jQuery object to insert before each
element in the set of matched elements.

" nil nil) ("content" "One or more additional DOM elements, arrays of elements, HTML strings,
or jQuery objects to insert before each element in the set of matched
elements.
" "true" nil)) (("function" "A function that returns an HTML string to insert before each element in
the set of matched elements.

" nil nil))) ("desc" (text . "Insert content, specified by the parameter, before each element in the
set of matched elements.

")) ("longdesc" (text . "The .before() and .insertBefore() methods perform the same task. The
major difference is in the syntax-specifically, in the placement of the
content and target. With .before(), the selector expression preceding
the method is the container before which the content is inserted. With
.insertBefore(), on the other hand, the content precedes the method,
either as a selector expression or as markup created on the fly, and it
is inserted before the target container.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<div class=\"container\">
			      <h2>Greetings</h2>
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "You can create content and insert it before several elements at once:


") (text . "") (js . "$('.inner').before('<p>Test</p>');") (text . "") (text . "Each inner <div> element gets this new content:


") (text . "") (js . "<div class=\"container\">
			      <h2>Greetings</h2>
			      <p>Test</p>
			      <div class=\"inner\">Hello</div>
			      <p>Test</p>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "You can also select an element on the page and insert it before
another:

") (text . "") (js . "$('.container').before($('h2'));") (text . "") (text . "If an element selected this way is inserted elsewhere, it will be moved
before the target (not cloned):

") (text . "") (js . "<h2>Greetings</h2>
			      <div class=\"container\">
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "If there is more than one target element, however, cloned copies of the
inserted element will be created for each target after the first.

") (text . "") (text . "In jQuery 1.4, .before() and .after() will also work on disconnected
DOM nodes:

") (text . "") (js . "$(\"<div/>\").before(\"<p></p>\");") (text . "") (text . "The result is a jQuery set that contains a paragraph and a div (in that
order).

") (text . "") (text . " Additional Arguments


") (text . "") (text . "Similar to other content-adding methods such as .prepend() and .after()
, .before() also supports passing in multiple arguments as input.
Supported input includes DOM elements, jQuery objects, HTML strings,
and arrays of DOM elements.
") (text . "") (text . "For example, the following will insert two new <div>s and an existing
<div> before the first paragraph:

") (text . "") (js . "var $newdiv1 = $('<div id=\"object1\"/>'),
			      newdiv2 = document.createElement('div'),
			      existingdiv1 = document.getElementById('foo');

			      $('p').first().before($newdiv1, [newdiv2, existingdiv1]);
			      ") (text . "") (text . "Since .before() can accept any number of additional arguments, the same
result can be achieved by passing in the three <div>s as three separate
arguments, like so: $(ʼpʼ).first().before($newdiv1, newdiv2,
existingdiv1). The type and number of arguments will largely depend on
how you collect the elements in your code.
") (text . "")) ("examples" ((text . "") (text . "Inserts some HTML before all paragraphs.

") (text . "") (js . "$(\"p\").before(\"<b>Hello</b>\");") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<p> is what I said...</p>") (text . "")) ((text . "") (text . "Inserts a DOM element before all paragraphs.

") (text . "") (js . "$(\"p\").before( document.createTextNode(\"Hello\") );") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<p> is what I said...</p>") (text . "")) ((text . "") (text . "Inserts a jQuery object (similar to an Array of DOM Elements) before
all paragraphs.

") (text . "") (js . "$(\"p\").before( $(\"b\") );") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<p> is what I said...</p><b>Hello</b>") (text . ""))))) jquery-doc-hash)

(push "insertAfter" jquery-methods)

(puthash "insertAfter" (quote (("name" . "insertAfter") ("signatures" "insertAfter" (("target" "A selector, element, HTML string, or jQuery object; the matched set of
elements will be inserted after the element(s) specified by this
parameter.
" nil nil))) ("desc" (text . "Insert every element in the set of matched elements after the target.

")) ("longdesc" (text . "The .after() and .insertAfter() methods perform the same task. The
major difference is in the syntax-specifically, in the placement of the
content and target. With .after(), the selector expression preceding
the method is the container after which the content is inserted. With
.insertAfter(), on the other hand, the content precedes the method,
either as a selector expression or as markup created on the fly, and it
is inserted after the target container.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<div class=\"container\">
			      <h2>Greetings</h2>
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "We can create content and insert it after several elements at once:


") (text . "") (js . "$('<p>Test</p>').insertAfter('.inner');") (text . "") (text . "Each inner <div> element gets this new content:


") (text . "") (js . "<div class=\"container\">
			      <h2>Greetings</h2>
			      <div class=\"inner\">Hello</div>
			      <p>Test</p>
			      <div class=\"inner\">Goodbye</div>
			      <p>Test</p>
			      </div>") (text . "") (text . "We can also select an element on the page and insert it after another:


") (text . "") (js . "$('h2').insertAfter($('.container'));") (text . "") (text . "If an element selected this way is inserted elsewhere, it will be moved
after the target (not cloned):

") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>
			      <h2>Greetings</h2>") (text . "") (text . "If there is more than one target element, however, cloned copies of the
inserted element will be created for each target after the first.

")) ("examples" ((text . "") (text . "Inserts all paragraphs after an element with id of \"foo\". Same as
$(\"#foo\").after(\"p\")

") (text . "") (js . "$(\"p\").insertAfter(\"#foo\"); // check after() examples") (text . "") (css . "#foo { background:yellow; }") (text . "") (html . "<p> is what I said... </p><div id=\"foo\">FOO!</div>") (text . ""))))) jquery-doc-hash)

(push "after" jquery-methods)

(puthash "after" (quote (("name" . "after") ("signatures" "after" (("content" "HTML string, DOM element, or jQuery object to insert after each element
in the set of matched elements.

" nil nil) ("content" "One or more additional DOM elements, arrays of elements, HTML strings,
or jQuery objects to insert after each element in the set of matched
elements.
" "true" nil)) (("function(index)" "A function that returns an HTML string to insert after each element in
the set of matched elements.

" nil nil))) ("desc" (text . "Insert content, specified by the parameter, after each element in the
set of matched elements.

")) ("longdesc" (text . "The .after() and .insertAfter() methods perform the same task. The
major difference is in the syntaxâspecifically, in the placement of the
content and target. With .after(), the selector expression preceding
the method is the container after which the content is inserted. With
.insertAfter(), on the other hand, the content precedes the method,
either as a selector expression or as markup created on the fly, and it
is inserted after the target container.
") (text . "") (text . "Using the following HTML:


") (text . "") (js . "<div class=\"container\">
			      <h2>Greetings</h2>
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>") (text . "") (text . "Content can be created and then inserted after several elements at
once:

") (text . "") (js . "$('.inner').after('<p>Test</p>');") (text . "") (text . "Each inner <div> element gets this new content:


") (text . "") (js . "<div class=\"container\">
			      <h2>Greetings</h2>
			      <div class=\"inner\">Hello</div>
			      <p>Test</p>
			      <div class=\"inner\">Goodbye</div>
			      <p>Test</p>
			      </div>") (text . "") (text . "An element in the DOM can also be selected and inserted after another
element:

") (text . "") (js . "$('.container').after($('h2'));") (text . "") (text . "If an element selected this way is inserted elsewhere, it will be moved
rather than cloned:

") (text . "") (js . "<div class=\"container\">
			      <div class=\"inner\">Hello</div>
			      <div class=\"inner\">Goodbye</div>
			      </div>
			      <h2>Greetings</h2>") (text . "") (text . "If there is more than one target element, however, cloned copies of the
inserted element will be created for each target after the first.

") (text . "") (text . " Inserting Disconnected DOM nodes


") (text . "") (text . "As of jQuery 1.4, .before() and .after() will also work on disconnected
DOM nodes. For example, given the following code:

") (text . "") (js . "$('<div/>').after('<p></p>');") (text . "") (text . "The result is a jQuery set containing a div and a paragraph, in that
order. That set can be further manipulated, even before it is inserted
in the document.
") (text . "") (js . "$('<div/>').after('<p></p>').addClass('foo')
			      .filter('p').attr('id', 'bar').html('hello')
			      .end()
			      .appendTo('body');") (text . "") (text . "This results in the following elements inserted just before the closing
</body> tag:

") (text . "") (js . "
				<div class=\"foo\"></div>
				<p class=\"foo\" id=\"bar\">hello</p>
			      ") (text . "") (text . " Passing a Function


") (text . "") (text . "As of jQuery 1.4, .after() supports passing a function that returns the
elements to insert.

") (text . "") (js . "$('p').after(function() {
			      return '<div>' + this.className + '</div>';
			      });") (text . "") (text . "This example inserts a <div> after each paragraph, with each new <div>
containing the class name(s) of its preceding paragraph.

") (text . "") (text . " Additional Arguments


") (text . "") (text . "Similar to other content-adding methods such as .prepend() and
.before() , .after() also supports passing in multiple arguments as
input. Supported input includes DOM elements, jQuery objects, HTML
strings, and arrays of DOM elements.
") (text . "") (text . "For example, the following will insert two new <div>s and an existing
<div> after the first paragraph:

") (text . "") (js . "var $newdiv1 = $('<div id=\"object1\"/>'),
			      newdiv2 = document.createElement('div'),
			      existingdiv1 = document.getElementById('foo');

			      $('p').first().after($newdiv1, [newdiv2, existingdiv1]);
			      ") (text . "") (text . "Since .after() can accept any number of additional arguments, the same
result can be achieved by passing in the three <div>s as three separate
arguments, like so: $(ʼpʼ).first().after($newdiv1, newdiv2,
existingdiv1). The type and number of arguments will largely depend on
the elements are collected in the code.
") (text . "")) ("examples" ((text . "") (text . "Inserts some HTML after all paragraphs.

") (text . "") (js . "$(\"p\").after(\"<b>Hello</b>\");") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<p>I would like to say: </p>") (text . "")) ((text . "") (text . "Inserts a DOM element after all paragraphs.

") (text . "") (js . "$(\"p\").after( document.createTextNode(\"Hello\") );") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<p>I would like to say: </p>") (text . "")) ((text . "") (text . "Inserts a jQuery object (similar to an Array of DOM Elements) after all
paragraphs.

") (text . "") (js . "$(\"p\").after( $(\"b\") );") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<b>Hello</b><p>I would like to say: </p>") (text . ""))))) jquery-doc-hash)

(push "prependTo" jquery-methods)

(puthash "prependTo" (quote (("name" . "prependTo") ("signatures" "prependTo" (("target" "A selector, element, HTML string, or jQuery object; the matched set of
elements will be inserted at the beginning of the element(s) specified
by this parameter.
" nil nil))) ("desc" (text . "Insert every element in the set of matched elements to the beginning of
the target.

")) ("longdesc" (text . "The .prepend() and .prependTo() methods perform the same task. The
major difference is in the syntax-specifically, in the placement of the
content and target. With .prepend(), the selector expression preceding
the method is the container into which the content is inserted. With
.prependTo(), on the other hand, the content precedes the method,
either as a selector expression or as markup created on the fly, and it
is inserted into the target container.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<h2>Greetings</h2>
			    <div class=\"container\">
			    <div class=\"inner\">Hello</div>
			    <div class=\"inner\">Goodbye</div>
			    </div>") (text . "") (text . "We can create content and insert it into several elements at once:


") (text . "") (js . "$('<p>Test</p>').prependTo('.inner');") (text . "") (text . "Each inner <div> element gets this new content:


") (text . "") (js . "<h2>Greetings</h2>
			    <div class=\"container\">
			    <div class=\"inner\">
			    <p>Test</p>
			    Hello
			    </div>
			    <div class=\"inner\">
			    <p>Test</p>
			    Goodbye
			    </div>
			    </div>") (text . "") (text . "We can also select an element on the page and insert it into another:


") (text . "") (js . "$('h2').prependTo($('.container'));") (text . "") (text . "If an element selected this way is inserted elsewhere, it will be moved
into the target (not cloned):

") (text . "") (js . "<div class=\"container\">
			    <h2>Greetings</h2>
			    <div class=\"inner\">Hello</div>
			    <div class=\"inner\">Goodbye</div>
			    </div>") (text . "") (text . "If there is more than one target element, however, cloned copies of the
inserted element will be created for each target after the first.

")) ("examples" ((text . "") (text . "Prepends all spans to the element with the ID \"foo\"

") (text . "") (css . "div { background:yellow; }") (text . "") (js . "$(\"span\").prependTo(\"#foo\"); // check prepend() examples") (text . "") (html . "<div id=\"foo\">FOO!</div>
			      
			      <span>I have something to say... </span>") (text . ""))))) jquery-doc-hash)

(push "prepend" jquery-methods)

(puthash "prepend" (quote (("name" . "prepend") ("signatures" "prepend" (("content" "DOM element, array of elements, HTML string, or jQuery object to insert
at the beginning of each element in the set of matched elements.

" nil nil) ("content" "One or more additional DOM elements, arrays of elements, HTML strings,
or jQuery objects to insert at the beginning of each element in the set
of matched elements.
" "true" nil)) (("function(index, html)" "A function that returns an HTML string to insert at the beginning of
each element in the set of matched elements. Receives the index
position of the element in the set and the old HTML value of the
element as arguments.
" nil nil))) ("desc" (text . "Insert content, specified by the parameter, to the beginning of each
element in the set of matched elements.

")) ("longdesc" (text . "The .prepend() method inserts the specified content as the first child
of each element in the jQuery collection (To insert it as the last
child, use .append() ).
") (text . "") (text . "The .prepend() and .prependTo() methods perform the same task. The
major difference is in the syntaxâspecifically, in the placement of the
content and target. With .prepend(), the selector expression preceding
the method is the container into which the content is inserted. With
.prependTo(), on the other hand, the content precedes the method,
either as a selector expression or as markup created on the fly, and it
is inserted into the target container.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<h2>Greetings</h2>
			    <div class=\"container\">
			    <div class=\"inner\">Hello</div>
			    <div class=\"inner\">Goodbye</div>
			    </div>") (text . "") (text . "You can create content and insert it into several elements at once:


") (text . "") (js . "$('.inner').prepend('<p>Test</p>');") (text . "") (text . "Each <div class=\"inner\"> element gets this new content:


") (text . "") (js . "<h2>Greetings</h2>
			    <div class=\"container\">
			    <div class=\"inner\">
			    <p>Test</p>
			    Hello
			    </div>
			    <div class=\"inner\">
			    <p>Test</p>
			    Goodbye
			    </div>
			    </div>") (text . "") (text . "You can also select an element on the page and insert it into another:


") (text . "") (js . "$('.container').prepend($('h2'));") (text . "") (text . "If a single element selected this way is inserted elsewhere, it will be
moved into the target ( not cloned):

") (text . "") (js . "<div class=\"container\">
			    <h2>Greetings</h2>
			    <div class=\"inner\">Hello</div>
			    <div class=\"inner\">Goodbye</div>
			    </div>") (text . "") (text . "Important: If there is more than one target element, however, cloned
copies of the inserted element will be created for each target after
the first.
") (text . "") (text . " Additional Arguments


") (text . "") (text . "Similar to other content-adding methods such as .append() and .before()
, .prepend() also supports passing in multiple arguments as input.
Supported input includes DOM elements, jQuery objects, HTML strings,
and arrays of DOM elements.
") (text . "") (text . "For example, the following will insert two new <div>s and an existing
<div> as the first three child nodes of the body:

") (text . "") (js . "var $newdiv1 = $('<div id=\"object1\"/>'),
			    newdiv2 = document.createElement('div'),
			    existingdiv1 = document.getElementById('foo');

			    $('body').prepend($newdiv1, [newdiv2, existingdiv1]);
			    ") (text . "") (text . "Since .prepend() can accept any number of additional arguments, the
same result can be achieved by passing in the three <div>s as three
separate arguments, like so: $(ʼbodyʼ).prepend($newdiv1, newdiv2,
existingdiv1). The type and number of arguments will largely depend on
how you collect the elements in your code.
") (text . "")) ("examples" ((text . "") (text . "Prepends some HTML to all paragraphs.

") (text . "") (js . "$(\"p\").prepend(\"<b>Hello </b>\");") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<p>there, friend!</p>

			      <p>amigo!</p>") (text . "")) ((text . "") (text . "Prepends a DOM Element to all paragraphs.

") (text . "") (js . "$(\"p\").prepend(document.createTextNode(\"Hello \"));") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<p>is what I'd say</p>
			      <p>is what I said</p>") (text . "")) ((text . "") (text . "Prepends a jQuery object (similar to an Array of DOM Elements) to all
paragraphs.

") (text . "") (js . "$(\"p\").prepend( $(\"b\") );") (text . "") (css . "p { background:yellow; }") (text . "") (html . "<p> is what was said.</p><b>Hello</b>") (text . ""))))) jquery-doc-hash)

(push "appendTo" jquery-methods)

(puthash "appendTo" (quote (("name" . "appendTo") ("signatures" "appendTo" (("target" "A selector, element, HTML string, or jQuery object; the matched set of
elements will be inserted at the end of the element(s) specified by
this parameter.
" nil nil))) ("desc" (text . "Insert every element in the set of matched elements to the end of the
target.

")) ("longdesc" (text . "The .append() and .appendTo() methods perform the same task. The major
difference is in the syntax-specifically, in the placement of the
content and target. With .append(), the selector expression preceding
the method is the container into which the content is inserted. With
.appendTo(), on the other hand, the content precedes the method, either
as a selector expression or as markup created on the fly, and it is
inserted into the target container.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<h2>Greetings</h2>
			    <div class=\"container\">
			    <div class=\"inner\">Hello</div>
			    <div class=\"inner\">Goodbye</div>
			    </div>
			    ") (text . "") (text . "We can create content and insert it into several elements at once:


") (text . "") (js . "$('<p>Test</p>').appendTo('.inner');
			    ") (text . "") (text . "Each inner <div> element gets this new content:


") (text . "") (js . "<h2>Greetings</h2>
			    <div class=\"container\">
			    <div class=\"inner\">
			    Hello
			    <p>Test</p>
			    </div>
			    <div class=\"inner\">
			    Goodbye
			    <p>Test</p>
			    </div>
			    </div>
			    ") (text . "") (text . "We can also select an element on the page and insert it into another:


") (text . "") (js . "$('h2').appendTo($('.container'));
			    ") (text . "") (text . "If an element selected this way is inserted elsewhere, it will be moved
into the target (not cloned):

") (text . "") (js . "<div class=\"container\">
			    <div class=\"inner\">Hello</div>
			    <div class=\"inner\">Goodbye</div>
			    <h2>Greetings</h2>
			    </div>
			    ") (text . "") (text . "If there is more than one target element, however, cloned copies of the
inserted element will be created for each target after the first.

")) ("examples" ((text . "") (text . "Appends all spans to the element with the ID \"foo\"

") (text . "") (js . "$(\"span\").appendTo(\"#foo\"); // check append() examples") (text . "") (css . "#foo { background:yellow; }") (text . "") (html . "<span>I have nothing more to say... </span>

			      <div id=\"foo\">FOO! </div>") (text . ""))))) jquery-doc-hash)

(push "append" jquery-methods)

(puthash "append" (quote (("name" . "append") ("signatures" "append" (("content" "DOM element, HTML string, or jQuery object to insert at the end of each
element in the set of matched elements.

" nil nil) ("content" "One or more additional DOM elements, arrays of elements, HTML strings,
or jQuery objects to insert at the end of each element in the set of
matched elements.
" "true" nil)) (("function(index, html)" "A function that returns an HTML string to insert at the end of each
element in the set of matched elements. Receives the index position of
the element in the set and the old HTML value of the element as
arguments.
" nil nil))) ("desc" (text . "Insert content, specified by the parameter, to the end of each element
in the set of matched elements.

")) ("longdesc" (text . "The .append() method inserts the specified content as the last child of
each element in the jQuery collection (To insert it as the first child,
use .prepend() ).
") (text . "") (text . "The .append() and .appendTo() methods perform the same task. The major
difference is in the syntax-specifically, in the placement of the
content and target. With .append(), the selector expression preceding
the method is the container into which the content is inserted. With
.appendTo(), on the other hand, the content precedes the method, either
as a selector expression or as markup created on the fly, and it is
inserted into the target container.
") (text . "") (text . "Consider the following HTML:


") (text . "") (js . "<h2>Greetings</h2>
			    <div class=\"container\">
			    <div class=\"inner\">Hello</div>
			    <div class=\"inner\">Goodbye</div>
			    </div>
			    ") (text . "") (text . "You can create content and insert it into several elements at once:


") (text . "") (js . "$('.inner').append('<p>Test</p>');
			    ") (text . "") (text . "Each inner <div> element gets this new content:


") (text . "") (js . "<h2>Greetings</h2>
			    <div class=\"container\">
			    <div class=\"inner\">
			    Hello
			    <p>Test</p>
			    </div>
			    <div class=\"inner\">
			    Goodbye
			    <p>Test</p>
			    </div>
			    </div>
			    ") (text . "") (text . "You can also select an element on the page and insert it into another:


") (text . "") (js . "$('.container').append($('h2'));
			    ") (text . "") (text . "If an element selected this way is inserted elsewhere, it will be moved
into the target (not cloned):

") (text . "") (js . "<div class=\"container\">
			    <div class=\"inner\">Hello</div>
			    <div class=\"inner\">Goodbye</div>
			    <h2>Greetings</h2>
			    </div>
			    ") (text . "") (text . "If there is more than one target element, however, cloned copies of the
inserted element will be created for each target after the first.

") (text . "") (text . " Additional Arguments


") (text . "") (text . "Similar to other content-adding methods such as .prepend() and
.before() , .append() also supports passing in multiple arguments as
input. Supported input includes DOM elements, jQuery objects, HTML
strings, and arrays of DOM elements.
") (text . "") (text . "For example, the following will insert two new <div>s and an existing
<div> as the last three child nodes of the body:

") (text . "") (js . "var $newdiv1 = $('<div id=\"object1\"/>'),
			    newdiv2 = document.createElement('div'),
			    existingdiv1 = document.getElementById('foo');

			    $('body').append($newdiv1, [newdiv2, existingdiv1]);
			    ") (text . "") (text . "Since .append() can accept any number of additional arguments, the same
result can be achieved by passing in the three <div>s as three separate
arguments, like so: $(ʼbodyʼ).append($newdiv1, newdiv2, existingdiv1).
The type and number of arguments will largely depend on how you collect
the elements in your code.
") (text . "")) ("examples" ((text . "") (text . "Appends some HTML to all paragraphs.

") (text . "") (js . "
			      $(\"p\").append(\"<strong>Hello</strong>\");
			      ") (text . "") (css . "
			      p { background:yellow; }
			      ") (text . "") (html . "<p>I would like to say: </p>") (text . "")) ((text . "") (text . "Appends an Element to all paragraphs.

") (text . "") (js . "
			      $(\"p\").append(document.createTextNode(\"Hello\"));
			      ") (text . "") (css . "
			      p { background:yellow; }
			      ") (text . "") (html . "<p>I would like to say: </p>
			      ") (text . "")) ((text . "") (text . "Appends a jQuery object (similar to an Array of DOM Elements) to all
paragraphs.

") (text . "") (js . "
			      $(\"p\").append( $(\"strong\") );
			      ") (text . "") (css . "
			      p { background:yellow; }
			      ") (text . "") (html . "<strong>Hello world!!!</strong><p>I would like to say: </p>") (text . ""))))) jquery-doc-hash)

(push "val" jquery-methods)

(puthash "val" (quote (("name" . "val") ("signatures" "val" nil) ("desc" (text . "Get the current value of the first element in the set of matched
elements.

")) ("longdesc" (text . "") (text . "The .val() method is primarily used to get the values of form elements.
In the case of <select multiple=\"multiple\"> elements, the .val() method
returns an array containing each selected option.
") (text . "") (text . "For selects and checkboxes, you can also use the :selected and :checked
selectors to get at values, for example:

") (text . "") (js . "$('select.foo option:selected').val();    // get the value from a dropdown select
			      $('select.foo').val();                    // get the value from a dropdown select even easier
			      $('input:checkbox:checked').val();        // get the value from a checked checkbox
			    $('input:radio[name=bar]:checked').val(); // get the value from a set of radio buttons")) ("examples" ((text . "") (text . "Get the single value from a single select and an array of values from a
multiple select and display their values.

") (text . "") (js . "
			      function displayVals() {
			      var singleValues = $(\"#single\").val();
			      var multipleValues = $(\"#multiple\").val() || [];
			      $(\"p\").html(\"<b>Single:</b> \" + 
			      singleValues +
			      \" <b>Multiple:</b> \" + 
			      multipleValues.join(\", \"));
			      }
			      
			      $(\"select\").change(displayVals);
			      displayVals();
			      
			      ") (text . "") (css . "
			      p { color:red; margin:4px; }
			      b { color:blue; }
			      ") (text . "") (html . "<p></p>
			      <select id=\"single\">
			      <option>Single</option>
			      <option>Single2</option>
			      
			      </select>
			      <select id=\"multiple\" multiple=\"multiple\">
			      <option selected=\"selected\">Multiple</option>
			      <option>Multiple2</option>
			      
			      <option selected=\"selected\">Multiple3</option>
			      </select>") (text . "")) ((text . "") (text . "Find the value of an input box.

") (text . "") (js . "
			      $(\"input\").keyup(function () {
			      var value = $(this).val();
			      $(\"p\").text(value);
			      }).keyup();
			      ") (text . "") (css . "
			      
			      p { color:blue; margin:8px; }
			      ") (text . "") (html . "<input type=\"text\" value=\"some text\"/>
			      <p></p>") (text . ""))))) jquery-doc-hash)

(push "val" jquery-methods)

(puthash "val" (quote (("name" . "val") ("signatures" "val" (("value" "A string of text or an array of strings corresponding to the value of
each matched element to set as selected/checked.

" nil nil)) (("function(index, value)" "A function returning the value to set.

" nil nil))) ("desc" (text . "Set the value of each element in the set of matched elements.

")) ("longdesc" (text . "This method is typically used to set the values of form fields.


") (text . "") (text . "Passing an array of element values allows matching <input
type=\"checkbox\">, <input type=\"radio\"> and <option>s inside of n
<select multiple=\"multiple\"> to be selected. In the case of <input
type=\"radio\">s that are part of a radio group and <select
multiple=\"multiple\"> the other elements will be deselected.
") (text . "") (text . "The .val() method allows us to set the value by passing in a function.
As of jQuery 1.4, the function is passed two arguments, the current
elementʼs index and its current value:
") (text . "") (js . "$('input:text.items').val(function(index, value) {
			    return value + ' ' + this.className;
			    });
			    ") (text . "") (text . "This example appends the string \" items\" to the text inputsʼ values.


") (text . "")) ("examples" ((text . "") (text . "Set the value of an input box.

") (text . "") (js . "
			      $(\"button\").click(function () {
			      var text = $(this).text();
			      $(\"input\").val(text);
			      });
			      ") (text . "") (css . "
			      button { margin:4px; cursor:pointer; }
			      input { margin:4px; color:blue; }
			      ") (text . "") (html . "<div>
			      <button>Feed</button>
			      <button>the</button>

			      <button>Input</button>
			      </div>
			      <input type=\"text\" value=\"click a button\" />") (text . "")) ((text . "") (text . "Use the function argument to modify the value of an input box.

") (text . "") (js . "
			      $('input').bind('blur', function() {
			      
			      $(this).val(function(i, val) {
			      return val.toUpperCase();
			      });
			      
			      });
			      ") (text . "") (html . "
			      <p>Type something and then click or tab out of the input.</p>
			      <input type=\"text\" value=\"type something\" />
			      ") (text . "")) ((text . "") (text . "Set a single select, a multiple select, checkboxes and a radio button .


") (text . "") (js . "
			      
			      $(\"#single\").val(\"Single2\");
			      $(\"#multiple\").val([\"Multiple2\", \"Multiple3\"]); 
			      $(\"input\").val([\"check1\",\"check2\", \"radio1\" ]);

			      ") (text . "") (css . "
			      body { color:blue; }
			      ") (text . "") (html . "<select id=\"single\">
			      <option>Single</option>
			      <option>Single2</option>
			      </select>

			      <select id=\"multiple\" multiple=\"multiple\">
			      <option selected=\"selected\">Multiple</option>
			      <option>Multiple2</option>

			      <option selected=\"selected\">Multiple3</option>
			      </select><br/>
			      <input type=\"checkbox\" name=\"checkboxname\" value=\"check1\"/> check1
			      <input type=\"checkbox\" name=\"checkboxname\" value=\"check2\"/> check2
			      <input type=\"radio\"  name=\"r\" value=\"radio1\"/> radio1
			      <input type=\"radio\"  name=\"r\" value=\"radio2\"/> radio2") (text . ""))))) jquery-doc-hash)

(push "text" jquery-methods)

(puthash "text" (quote (("name" . "text") ("signatures" "text" nil) ("desc" (text . "Get the combined text contents of each element in the set of matched
elements, including their descendants.

")) ("longdesc" (text . "Unlike the .html() method, .text() can be used in both XML and HTML
documents. The result of the .text() method is a string containing the
combined text of all matched elements. (Due to variations in the HTML
parsers in different browsers, the text returned may vary in newlines
and other white space.) Consider the following HTML:
") (text . "") (js . "<div class=\"demo-container\">
			    <div class=\"demo-box\">Demonstration Box</div>
			    <ul>
			    <li>list item 1</li>
			    <li>list <strong>item</strong> 2</li>
			    </ul>
			    </div>
			    ") (text . "") (text . "The code $(ʼdiv.demo-containerʼ).text() would produce the following
result:

") (text . "") (text . "Demonstration Box list item 1 list item 2


") (text . "") (text . "The .text() method cannot be used on input elements. For input field
text, use the .val() method.

") (text . "") (text . "As of jQuery 1.4, the .text() method returns the value of text and
CDATA nodes as well as element nodes.

") (text . "")) ("examples" ((text . "") (text . "Find the text in the first paragraph (stripping out the html), then set
the html of the last paragraph to show it is just text (the red bold is
gone).
") (text . "") (js . "
			      var str = $(\"p:first\").text();
			      $(\"p:last\").html(str);
			      ") (text . "") (css . "
			      p { color:blue; margin:8px; }
			      b { color:red; }
			      ") (text . "") (html . "<p><b>Test</b> Paragraph.</p>

			      <p></p>") (text . ""))))) jquery-doc-hash)

(push "text" jquery-methods)

(puthash "text" (quote (("name" . "text") ("signatures" "text" (("textString" "A string of text to set as the content of each matched element.

" nil nil)) (("function(index, text)" "A function returning the text content to set. Receives the index
position of the element in the set and the old text value as arguments.

" nil nil))) ("desc" (text . "Set the content of each element in the set of matched elements to the
specified text.

")) ("longdesc" (text . "Unlike the .html() method, .text() can be used in both XML and HTML
documents.

") (text . "") (text . "We need to be aware that this method escapes the string provided as
necessary so that it will render correctly in HTML. To do so, it calls
the DOM method .createTextNode(), which replaces special characters
with their HTML entity equivalents (such as &lt; for <). Consider the
following HTML:
") (text . "") (js . "<div class=\"demo-container\">
			    <div class=\"demo-box\">Demonstration Box</div>
			    <ul>
			    <li>list item 1</li>
			    <li>list <strong>item</strong> 2</li>
			    </ul>
			    </div>
			    ") (text . "") (text . "The code $(ʼdiv.demo-containerʼ).text(ʼ<p>This is a test.</p>ʼ); will
produce the following DOM output:

") (text . "") (js . "<div class=\"demo-container\">
			    &lt;p&gt;This is a test.&lt;/p&gt;
			    </div>") (text . "") (text . "It will appear on a rendered page as though the tags were exposed, like
this:

") (text . "") (js . "<p>This is a test</p>") (text . "") (text . "The .text() method cannot be used on input elements. For input field
text, use the .val() method.

") (text . "") (text . "As of jQuery 1.4, the .text() method allows us to set the text content
by passing in a function.

") (text . "") (js . "$('ul li').text(function(index) {
			    return 'item number ' + (index + 1);
			    });") (text . "") (text . "Given an unordered list with three <li> elements, this example will
produce the following DOM output:

") (text . "") (js . "<ul>
			    <li>item number 1</li>
			    <li>item number 2</li>
			    <li>item number 3</li>
			    </ul>
			    ") (text . "")) ("examples" ((text . "") (text . "Add text to the paragraph (notice the bold tag is escaped).

") (text . "") (js . "$(\"p\").text(\"<b>Some</b> new text.\");") (text . "") (css . "

			      p { color:blue; margin:8px; }
			      ") (text . "") (html . "<p>Test Paragraph.</p>") (text . ""))))) jquery-doc-hash)

(push "html" jquery-methods)

(puthash "html" (quote (("name" . "html") ("signatures" "html" nil) ("desc" (text . "Get the HTML contents of the first element in the set of matched
elements.

")) ("longdesc" (text . "") (text . "This method is not available on XML documents.


") (text . "") (text . "In an HTML document, .html() can be used to get the contents of any
element. If the selector expression matches more than one element, only
the first match will have its HTML content returned. Consider this
code:
") (text . "") (js . "$('div.demo-container').html();") (text . "") (text . "In order for the following <div>ʼs content to be retrieved, it would
have to be the first one with class=\"demo-container\" in the document:

") (text . "") (js . "<div class=\"demo-container\">
			      <div class=\"demo-box\">Demonstration Box</div>
			      </div>") (text . "") (text . "The result would look like this:


") (text . "") (js . "<div class=\"demo-box\">Demonstration Box</div>") (text . "") (text . "This method uses the browserʼs innerHTML property. Some browsers may
not return HTML that exactly replicates the HTML source in an original
document. For example, Internet Explorer sometimes leaves off the
quotes around attribute values if they contain only alphanumeric
characters.
") (text . "")) ("examples" ((text . "") (text . "Click a paragraph to convert it from html to text.

") (text . "") (js . "
			      $(\"p\").click(function () {
			      var htmlStr = $(this).html();
			      $(this).text(htmlStr);
			      });
			      ") (text . "") (css . "
			      p { margin:8px; font-size:20px; color:blue; 
			      cursor:pointer; }
			      b { text-decoration:underline; }
			      button { cursor:pointer; }
			      ") (text . "") (html . "<p>
			      
			      <b>Click</b> to change the <span id=\"tag\">html</span>
			      </p>
			      <p>
			      
			      to a <span id=\"text\">text</span> node.
			      </p>
			      <p>
			      This <button name=\"nada\">button</button> does nothing.
			      </p>") (text . ""))))) jquery-doc-hash)

(push "html" jquery-methods)

(puthash "html" (quote (("name" . "html") ("signatures" "html" (("htmlString" "A string of HTML to set as the content of each matched element.

" nil nil)) (("function(index, oldhtml)" "A function returning the HTML content to set. Receives the index
position of the element in the set and the old HTML value as arguments.
jQuery empties the element before calling the function; use the oldhtml
argument to reference the previous content.
" nil nil))) ("desc" (text . "Set the HTML contents of each element in the set of matched elements.

")) ("longdesc" (text . "") (text . "The .html() method is not available in XML documents.


") (text . "") (text . "When .html() is used to set an elementʼs content, any content that was
in that element is completely replaced by the new content. Consider the
following HTML:
") (text . "") (js . "<div class=\"demo-container\">
			      <div class=\"demo-box\">Demonstration Box</div>
			      </div>") (text . "") (text . "The content of <div class=\"demo-container\"> can be set like this:


") (text . "") (js . "$('div.demo-container')
			      .html('<p>All new content. <em>You bet!</em></p>');") (text . "") (text . "That line of code will replace everything inside <div
class=\"demo-container\">:

") (text . "") (js . "<div class=\"demo-container\">
			      <p>All new content. <em>You bet!</em></p>
			      </div>") (text . "") (text . "As of jQuery 1.4, the .html() method allows the HTML content to be set
by passing in a function.

") (text . "") (js . "$('div.demo-container').html(function() {
			      var emph = '<em>' + $('p').length + ' paragraphs!</em>';
			      return '<p>All new content for ' + emph + '</p>';
			      });") (text . "") (text . "Given a document with six paragraphs, this example will set the HTML of
<div class=\"demo-container\"> to <p>All new content for <em>6
paragraphs!</em></p>.
") (text . "") (text . "This method uses the browserʼs innerHTML property. Some browsers may
not generate a DOM that exactly replicates the HTML source provided.
For example, Internet Explorer prior to version 8 will convert all href
properties on links to absolute URLs, and Internet Explorer prior to
version 9 will not correctly handle HTML5 elements without the addition
of a separate compatibility layer.
") (text . "")) ("examples" ((text . "") (text . "Add some html to each div.

") (text . "") (js . "$(\"div\").html(\"<span class='red'>Hello <b>Again</b></span>\");") (text . "") (css . "
			      
			      .red { color:red; }
			      ") (text . "") (html . "<span>Hello</span>
			      <div></div>
			      <div></div>
			      <div></div>") (text . "")) ((text . "") (text . "Add some html to each div then immediately do further manipulations to
the inserted html.

") (text . "") (js . "
			      
			      $(\"div\").html(\"<b>Wow!</b> Such excitement...\");
			      $(\"div b\").append(document.createTextNode(\"!!!\"))
			      .css(\"color\", \"red\");
			      
			      ") (text . "") (css . "
			      div { color:blue; font-size:18px; }
			      ") (text . "") (html . "<div></div>
			      <div></div>
			      <div></div>") (text . ""))))) jquery-doc-hash)

(push "map" jquery-methods)

(puthash "map" (quote (("name" . "map") ("signatures" "map" (("callback(index, domElement)" "A function object that will be invoked for each element in the current
set.

" nil nil))) ("desc" (text . "Pass each element in the current matched set through a function,
producing a new jQuery object containing the return values.

")) ("longdesc" (text . "As the return value is a jQuery-wrapped array, itʼs very common to
get() the returned object to work with a basic array.

") (text . "The .map() method is particularly useful for getting or setting the
value of a collection of elements. Consider a form with a set of
checkboxes in it:
") (text . "") (js . "
			      <form method=\"post\" action=\"\">
			      <fieldset>
			      <div>
			      <label for=\"two\">2</label>
			      <input type=\"checkbox\" value=\"2\" id=\"two\" name=\"number[]\">
			      </div>
			      <div>
			      <label for=\"four\">4</label>
			      <input type=\"checkbox\" value=\"4\" id=\"four\" name=\"number[]\">
			      </div>
			      <div>
			      <label for=\"six\">6</label>
			      <input type=\"checkbox\" value=\"6\" id=\"six\" name=\"number[]\">
			      </div>
			      <div>
			      <label for=\"eight\">8</label>
			      <input type=\"checkbox\" value=\"8\" id=\"eight\" name=\"number[]\">
			      </div>
			      </fieldset>
			      </form>
			    ") (text . "") (text . "We can get a comma-separated list of checkbox IDs:


") (text . "") (js . "$(':checkbox').map(function() {
			    return this.id;
			    }).get().join(',');") (text . "") (text . "The result of this call is the string, \"two,four,six,eight\".


") (text . "") (text . "Within the callback function, this refers to the current DOM element
for each iteration. The function can return an individual data item or
an array of data items to be inserted into the resulting set. If an
array is returned, the elements inside the array are inserted into the
set. If the function returns null or undefined, no element will be
inserted.
") (text . "")) ("examples" ((text . "") (text . "Build a list of all the values within a form.

") (text . "") (js . "
			      $(\"p\").append( $(\"input\").map(function(){
			      return $(this).val();
			      }).get().join(\", \") );

			      ") (text . "") (css . "
			      p { color:red; }
			      ") (text . "") (html . "<p><b>Values: </b></p>
			      <form>
			      <input type=\"text\" name=\"name\" value=\"John\"/>

			      <input type=\"text\" name=\"password\" value=\"password\"/>
			      <input type=\"text\" name=\"url\" value=\"http://ejohn.org/\"/>

			      </form>") (text . "")) ((text . "") (text . "A contrived example to show some functionality.

") (text . "") (js . "
			      var mappedItems = $(\"li\").map(function (index) {
			      var replacement = $(\"<li>\").text($(this).text()).get(0);
			      if (index == 0) {
			      // make the first item all caps
			      $(replacement).text($(replacement).text().toUpperCase());
			      } else if (index == 1 || index == 3) {
			      // delete the second and fourth items
			      replacement = null;
			      } else if (index == 2) {
			      // make two of the third item and add some text
			      replacement = [replacement,$(\"<li>\").get(0)];
			      $(replacement[0]).append(\"<b> - A</b>\");
			      $(replacement[1]).append(\"Extra <b> - B</b>\");
			      }

			      // replacement will be an dom element, null, 
			      // or an array of dom elements
			      return replacement;
			      });
			      $(\"#results\").append(mappedItems);

			      ") (text . "") (css . "
			      body { font-size:16px; }
			      ul { float:left; margin:0 30px; color:blue; }
			      #results { color:red; }
			      ") (text . "") (html . "<ul>
			      <li>First</li>
			      <li>Second</li>
			      <li>Third</li>

			      <li>Fourth</li>
			      <li>Fifth</li>
			      </ul>
			      <ul id=\"results\">

			      </ul>") (text . "")) ((text . "") (text . "Equalize the heights of the divs.

") (text . "") (js . "
			      $.fn.equalizeHeights = function(){
			      return this.height( Math.max.apply(this, $(this).map(function(i,e){ return $(e).height() }).get() ) )
			      }
			      $('input').click(function(){
			      $('div').equalizeHeights();
			      });

			      ") (text . "") (css . "
			      div { width: 40px; float:left; }
			      input { clear:left}
			      ") (text . "") (html . "

			      <input type=\"button\" value=\"equalize div heights\">

			      <div style=\"background:red; height: 40px; \"></div>
			      <div style=\"background:green; height: 70px;\"></div>
			      <div style=\"background:blue; height: 50px; \"></div>

			      ") (text . ""))))) jquery-doc-hash)

(push "is" jquery-methods)

(puthash "is" (quote (("name" . "is") ("signatures" "is" (("selector" "A string containing a selector expression to match elements against.

" nil nil)) (("function(index)" "A function used as a test for the set of elements. It accepts one
argument, index, which is the elementʼs index in the jQuery
collection.Within the function, this refers to the current DOM element.
" nil nil)) (("jQuery object" "An existing jQuery object to match the current set of elements against.


" nil nil)) (("element" "An element to match the current set of elements against.

" nil nil))) ("desc" (text . "Check the current matched set of elements against a selector, element,
or jQuery object and return true if at least one of these elements
matches the given arguments.
")) ("longdesc" (text . "") (text . "Unlike other filtering methods, .is() does not create a new jQuery
object. Instead, it allows you to test the contents of a jQuery object
without modification. This is often useful inside callbacks, such as
event handlers.
") (text . "") (text . "Suppose you have a list, with two of its items containing a child
element:

") (text . "") (js . "
				<ul>
				<li>list <strong>item 1</strong></li>
				<li><span>list item 2</span></li>
				<li>list item 3</li>
				</ul>
			      ") (text . "") (text . "You can attach a click handler to the <ul> element, and then limit the
code to be triggered only when a list item itself, not one of its
children, is clicked:
") (text . "") (js . "$(\"ul:).click(function(event) {
			      var $target = $(event.target);
			      if ( $target.is(\"li\") ) {
			      $target.css(\"background-color\", \"red\");
			      }
			      });") (text . "") (text . "Now, when the user clicks on the word \"list\" in the first item or
anywhere in the third item, the clicked list item will be given a red
background. However, when the user clicks on item 1 in the first item
or anywhere in the second item, nothing will occur, because in those
cases the target of the event would be <strong> or <span>,
respectively.
") (text . "") (text . " Using a Function


") (text . "") (text . "The second form of this method evaluates expressions related to
elements based on a function rather than a selector. For each element,
if the function returns true, .is() returns true as well. For example,
given a somewhat more involved HTML snippet:
") (text . "") (js . "
				<ul>
				<li><strong>list</strong> item 1 - one strong tag</li>
				<li><strong>list</strong> item <strong>2</strong> -
				two <span>strong tags</span></li>
				<li>list item 3</li>
				<li>list item 4</li>
				<li>list item 5</li>
				</ul>
			      ") (text . "") (text . "You can attach a click handler to every <li> that evaluates the number
of <strong> elements within the clicked <li> at that time like so:

") (text . "") (js . "
				$(\"li\").click(function() {
				var $li = $(this),
				isWithTwo = $li.is(function() {
				return $('strong', this).length === 2;
				});
				if ( isWithTwo ) {
				$li.css(\"background-color\", \"green\");
				} else {
				$li.css(\"background-color\", \"red\");
				}
				});
			      ") (text . "")) ("examples" ((text . "") (text . "Shows a few ways is() can be used inside an event handler.

") (text . "") (js . "
			      $(\"div\").one('click', function () {
			      if ($(this).is(\":first-child\")) {
			      $(\"p\").text(\"It's the first div.\");
			      } else if ($(this).is(\".blue,.red\")) {
			      $(\"p\").text(\"It's a blue or red div.\");
			      } else if ($(this).is(\":contains('Peter')\")) {
			      $(\"p\").text(\"It's Peter!\");
			      } else {
			      $(\"p\").html(\"It's nothing <em>special</em>.\");
			      }
			      $(\"p\").hide().slideDown(\"slow\");
			      $(this).css({\"border-style\": \"inset\", cursor:\"default\"});
			      });
			      ") (text . "") (css . "
			      div { width:60px; height:60px; margin:5px; float:left;
			      border:4px outset; background:green; text-align:center; 
			      font-weight:bolder; cursor:pointer; }
			      .blue { background:blue; }
			      .red { background:red; }
			      span { color:white; font-size:16px; }
			      p { color:red; font-weight:bolder; background:yellow; 
			      margin:3px; clear:left; display:none; }
			      ") (text . "") (html . "<div></div>
			      <div class=\"blue\"></div>
			      <div></div>
			      <div class=\"red\"></div>

			      <div><br/><span>Peter</span></div>
			      <div class=\"blue\"></div>
			      <p>&nbsp;</p>") (text . "")) ((text . "") (text . "Returns true, because the parent of the input is a form element.

") (text . "") (js . "
			      var isFormParent = $(\"input[type='checkbox']\").parent().is(\"form\");
			      $(\"div\").text(\"isFormParent = \" + isFormParent);
			      ") (text . "") (css . "div { color:red; }") (text . "") (html . "<form><input type=\"checkbox\" /></form>
			      <div></div>") (text . "")) ((text . "") (text . "Returns false, because the parent of the input is a p element.

") (text . "") (js . "
			      var isFormParent = $(\"input[type='checkbox']\").parent().is(\"form\");
			      $(\"div\").text(\"isFormParent = \" + isFormParent);
			      ") (text . "") (css . "div { color:red; }") (text . "") (html . "<form><p><input type=\"checkbox\" /></p></form>
			      <div></div>") (text . "")) ((text . "") (text . "Checks against an existing collection of alternating list elements.
Blue, alternating list elements slide up while others turn red.

") (text . "") (js . "
			      var $alt = $(\"#browsers li:nth-child(2n)\").css(\"background\", \"#00FFFF\");
			      $('li').click(function() {
			      var $li = $(this);
			      if ( $li.is( $alt ) ) {
			      $li.slideUp();
			      } else {
			      $li.css(\"background\", \"red\");
			      }
			      });
			      ") (text . "") (css . "li { cursor:pointer; }") (text . "") (html . "
			      <ul id=\"browsers\">
			      <li>Chrome</li>
			      <li>Safari</li>
			      <li>Firefox</li>
			      <li>Opera</li>
			      </ul>") (text . "")) ((text . "") (text . "An alternate way to achieve the above example using an element rather
than a jQuery object. Checks against an existing collection of
alternating list elements. Blue, alternating list elements slide up
while others turn red.
") (text . "") (js . "
			      var $alt = $(\"#browsers li:nth-child(2n)\").css(\"background\", \"#00FFFF\");
			      $('li').click(function() {
			      if ( $alt.is( this ) ) {
			      $(this).slideUp();
			      } else {
			      $(this).css(\"background\", \"red\");
			      }
			      });
			      ") (text . "") (css . "li { cursor:pointer; }") (text . "") (html . "
			      <ul id=\"browsers\">
			      <li>Chrome</li>
			      <li>Safari</li>
			      <li>Firefox</li>
			      <li>Opera</li>
			      </ul>") (text . ""))))) jquery-doc-hash)

(push "eq" jquery-methods)

(puthash "eq" (quote (("name" . "eq") ("signatures" "eq" (("index" "An integer indicating the 0-based position of the element.

" nil nil)) (("-index" "An integer indicating the position of the element, counting backwards
from the last element in the set.

" nil nil))) ("desc" (text . "Reduce the set of matched elements to the one at the specified index.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the .eq()
method constructs a new jQuery object from one element within that set.
The supplied index identifies the position of this element in the set.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (js . "
			      <ul>
			      <li>list item 1</li>
			      <li>list item 2</li>
			      <li>list item 3</li>
			      <li>list item 4</li>
			      <li>list item 5</li>
			      </ul>
			    ") (text . "") (text . "We can apply this method to the set of list items:


") (text . "") (js . "
			      $('li').eq(2).css('background-color', 'red');
			    ") (text . "") (text . "The result of this call is a red background for item 3. Note that the
supplied index is zero-based, and refers to the position of the element
within the jQuery object, not within the DOM tree.
") (text . "") (text . "Providing a negative number indicates a position starting from the end
of the set, rather than the beginning. For example:

") (text . "") (js . "
			      $('li').eq(-2).css('background-color', 'red');
			    ") (text . "") (text . "This time list item 4 is turned red, since it is two from the end of
the set.

") (text . "") (text . "If an element cannot be found at the specified zero-based index, the
method constructs a new jQuery object with an empty set and a length
property of 0.
") (text . "") (js . "
			      $('li').eq(5).css('background-color', 'red');
			    ") (text . "") (text . "Here, none of the list items is turned red, since .eq(5) indicates the
sixth of five list items.

") (text . "")) ("examples" ((text . "") (text . "Turn the div with index 2 blue by adding an appropriate class.

") (text . "") (js . "

			      $(\"body\").find(\"div\").eq(2).addClass(\"blue\");
			      ") (text . "") (css . "
			      div { width:60px; height:60px; margin:10px; float:left;
			      border:2px solid blue; }
			      .blue { background:blue; }
			      ") (text . "") (html . "<div></div>
			      <div></div>
			      <div></div>

			      <div></div>
			      <div></div>
			      <div></div>") (text . ""))))) jquery-doc-hash)

(push "filter" jquery-methods)

(puthash "filter" (quote (("name" . "filter") ("signatures" "filter" (("selector" "A string containing a selector expression to match the current set of
elements against.

" nil nil)) (("function(index)" "A function used as a test for each element in the set. this is the
current DOM element.

" nil nil)) (("element" "An element to match the current set of elements against.

" nil nil)) (("jQuery object" "An existing jQuery object to match the current set of elements against.


" nil nil))) ("desc" (text . "Reduce the set of matched elements to those that match the selector or
pass the functionʼs test.

")) ("longdesc" (text . "Given a jQuery object that represents a set of DOM elements, the
.filter() method constructs a new jQuery object from a subset of the
matching elements. The supplied selector is tested against each
element; all elements matching the selector will be included in the
result.
") (text . "") (text . "Consider a page with a simple list on it:


") (text . "") (text . "We can apply this method to the set of list items:


") (text . "") (js . "
			      $('li').filter(':even').css('background-color', 'red');
			    ") (text . "") (text . "The result of this call is a red background for items 1, 3, and 5, as
they match the selector (recall that :even and :odd use 0-based
indexing).
") (text . "") (text . " Using a Filter Function


") (text . "") (text . "The second form of this method allows us to filter elements against a
function rather than a selector. For each element, if the function
returns true, the element will be included in the filtered set;
otherwise, it will be excluded. Suppose we have a somewhat more
involved HTML snippet:
") (text . "") (js . "
			      <ul>
			      <li><strong>list</strong> item 1 -
			      one strong tag</li>
			      <li><strong>list</strong> item <strong>2</strong> -
			      two <span>strong tags</span></li>
			      <li>list item 3</li>
			      <li>list item 4</li>
			      <li>list item 5</li>
			      <li>list item 6</li>
			      </ul>
			    ") (text . "") (text . "We can select the list items, then filter them based on their contents:


") (text . "") (js . "
			      $('li').filter(function(index) {
			      return $('strong', this).length == 1;
			      }).css('background-color', 'red');
			    ") (text . "") (text . "This code will alter the first list item only, as it contains exactly
one <strong> tag. Within the filter function, this refers to each DOM
element in turn. The parameter passed to the function tells us the
index of that DOM element within the set matched by the jQuery object.
") (text . "") (text . "We can also take advantage of the index passed through the function,
which indicates the 0-based position of the element within the
unfiltered set of matched elements:
") (text . "") (js . "
			      $('li').filter(function(index) {
			      return index % 3 == 2;
			      }).css('background-color', 'red');
			    ") (text . "") (text . "This alteration to the code will cause the third and sixth list items
to be highlighted, as it uses the modulus operator ( %) to select every
item with an index value that, when divided by 3, has a remainder of 2.
") (text . "")) ("examples" ((text . "") (text . "Change the color of all divs then put a border around only some of
them.

") (text . "") (js . "

			      $(\"div\").css(\"background\", \"#c8ebcc\")
			      .filter(\".middle\")
			      .css(\"border-color\", \"red\");
			      ") (text . "") (css . "
			      div { width:60px; height:60px; margin:5px; float:left; 
			      border:2px white solid;}
			      ") (text . "") (html . "<div></div>

			      <div class=\"middle\"></div>
			      <div class=\"middle\"></div>
			      <div class=\"middle\"></div>
			      <div class=\"middle\"></div>

			      <div></div>") (text . "")) ((text . "") (text . "Selects all paragraphs and removes those without a class \"selected\".

") (text . "") (js . "$(\"p\").filter(\".selected\")") (text . "")) ((text . "") (text . "Selects all paragraphs and removes those that arenʼt of class
\"selected\" or the first one.

") (text . "") (js . "$(\"p\").filter(\".selected, :first\")") (text . "")) ((text . "") (text . "Change the color of all divs then put a border to specific ones.

") (text . "") (js . "
			      $(\"div\").css(\"background\", \"#b4b0da\")
			      .filter(function (index) {
			      return index == 1 || $(this).attr(\"id\") == \"fourth\";
			      })
			      .css(\"border\", \"3px double red\");

			      ") (text . "") (css . "
			      div { width:60px; height:60px; margin:5px; float:left; 
			      border:3px white solid; }
			      ") (text . "") (html . "<div id=\"first\"></div>
			      <div id=\"second\"></div>
			      <div id=\"third\"></div>

			      <div id=\"fourth\"></div>
			      <div id=\"fifth\"></div>
			      <div id=\"sixth\"></div>") (text . "")) ((text . "") (text . "Remove all elements that have a descendant ol element

") (text . "") (js . "$(\"div\").filter(function(index) {
			      return $(\"ol\", this).length == 0;
			      });") (text . ""))))) jquery-doc-hash)

(push "toggleClass" jquery-methods)

(puthash "toggleClass" (quote (("name" . "toggleClass") ("signatures" "toggleClass" (("className" "One or more class names (separated by spaces) to be toggled for each
element in the matched set.

" nil nil)) (("className" "One or more class names (separated by spaces) to be toggled for each
element in the matched set.

" nil nil) ("switch" "A boolean value to determine whether the class should be added or
removed.

" nil nil)) (("function(index, class)" "A function that returns class names to be toggled in the class
attribute of each element in the matched set. Receives the index
position of the element in the set and the old class value as
arguments.
" nil nil) ("switch" "A boolean value to determine whether the class should be added or
removed.

" "true" nil))) ("desc" (text . "Add or remove one or more classes from each element in the set of
matched elements, depending on either the classʼs presence or the value
of the switch argument.
")) ("longdesc" (text . "This method takes one or more class names as its parameter. In the
first version, if an element in the matched set of elements already has
the class, then it is removed; if an element does not have the class,
then it is added. For example, we can apply .toggleClass() to a simple
<div>:
") (text . "") (js . "<div class=\"tumble\">Some text.</div>
			    ") (text . "") (text . "The first time we apply $(ʼdiv.tumbleʼ).toggleClass(ʼbounceʼ), we get
the following:

") (text . "") (js . "<div class=\"tumble bounce\">Some text.</div>
			    ") (text . "") (text . "The second time we apply $(ʼdiv.tumbleʼ).toggleClass(ʼbounceʼ), the
<div> class is returned to the single tumble value:

") (text . "") (js . "<div class=\"tumble\">Some text.</div>") (text . "") (text . "Applying .toggleClass(ʼbounce spinʼ) to the same <div> alternates
between <div class=\"tumble bounce spin\"> and <div class=\"tumble\">.

") (text . "") (text . "The second version of .toggleClass() uses the second parameter for
determining whether the class should be added or removed. If this
parameterʼs value is true, then the class is added; if false, the class
is removed. In essence, the statement:
") (text . "") (js . "$('#foo').toggleClass(className, addOrRemove);") (text . "") (text . "is equivalent to:


") (text . "") (js . "if (addOrRemove) {
			    $('#foo').addClass(className);
			    }
			    else {
			    $('#foo').removeClass(className);
			    }
			    ") (text . "") (text . "As of jQuery 1.4, the .toggleClass() method allows us to indicate the
class name to be toggled by passing in a function.

") (text . "") (js . "$('div.foo').toggleClass(function() {
			    if ($(this).parent().is('.bar')) {
			    return 'happy';
			    } else {
			    return 'sad';
			    }
			    });") (text . "") (text . "This example will toggle the happy class for <div class=\"foo\"> elements
if their parent element has a class of bar; otherwise, it will toggle
the sad class.
") (text . "")) ("examples" ((text . "") (text . "Toggle the class ʼhighlightʼ when a paragraph is clicked.

") (text . "") (js . "
			      $(\"p\").click(function () {
			      $(this).toggleClass(\"highlight\");
			      });
			      ") (text . "") (css . "

			      p { margin: 4px; font-size:16px; font-weight:bolder; 
			      cursor:pointer; }
			      .blue { color:blue; }
			      .highlight { background:yellow; }
			      ") (text . "") (html . "<p class=\"blue\">Click to toggle</p>
			      <p class=\"blue highlight\">highlight</p>
			      <p class=\"blue\">on these</p>
			      <p class=\"blue\">paragraphs</p>") (text . "")) ((text . "") (text . "Add the \"highlight\" class to the clicked paragraph on every third click
of that paragraph, remove it every first and second click.

") (text . "") (js . "
			      var count = 0;
			      $(\"p\").each(function() {
			      var $thisParagraph = $(this);
			      var count = 0;
			      $thisParagraph.click(function() {
			      count++;
			      $thisParagraph.find(\"span\").text('clicks: ' + count);
			      $thisParagraph.toggleClass(\"highlight\", count % 3 == 0);
			      });
			      });

			      ") (text . "") (css . "
			      p { margin: 4px; font-size:16px; font-weight:bolder; 
			      cursor:pointer; }
			      .blue { color:blue; }
			      .highlight { background:red; }
			      ") (text . "") (html . "<p class=\"blue\">Click to toggle (<span>clicks: 0</span>)</p>
			      <p class=\"blue highlight\">highlight (<span>clicks: 0</span>)</p>
			      <p class=\"blue\">on these (<span>clicks: 0</span>)</p>

			      <p class=\"blue\">paragraphs (<span>clicks: 0</span>)</p>") (text . ""))))) jquery-doc-hash)

(push "removeClass" jquery-methods)

(puthash "removeClass" (quote (("name" . "removeClass") ("signatures" "removeClass" (("className" "One or more space-separated classes to be removed from the class
attribute of each matched element.

" "true" nil)) (("function(index, class)" "A function returning one or more space-separated class names to be
removed. Receives the index position of the element in the set and the
old class value as arguments.
" nil nil))) ("desc" (text . "Remove a single class, multiple classes, or all classes from each
element in the set of matched elements.

")) ("longdesc" (text . "If a class name is included as a parameter, then only that class will
be removed from the set of matched elements. If no class names are
specified in the parameter, all classes will be removed.
") (text . "") (text . "More than one class may be removed at a time, separated by a space,
from the set of matched elements, like so:

") (text . "") (js . "$('p').removeClass('myClass yourClass')
			    ") (text . "") (text . "This method is often used with .addClass() to switch elementsʼ classes
from one to another, like so:

") (text . "") (js . "$('p').removeClass('myClass noClass').addClass('yourClass');
			    ") (text . "") (text . "Here, the myClass and noClass classes are removed from all paragraphs,
while yourClass is added.

") (text . "") (text . "To replace all existing classes with another class, we can use
.attr(ʼclassʼ, ʼnewClassʼ) instead.

") (text . "") (text . "As of jQuery 1.4, the .removeClass() method allows us to indicate the
class to be removed by passing in a function.

") (text . "") (js . "$('li:last').removeClass(function() {
			    return $(this).prev().attr('class');
			    });") (text . "") (text . "This example removes the class name of the penultimate <li> from the
last <li>.

") (text . "")) ("examples" ((text . "") (text . "Remove the class ʼblueʼ from the matched elements.

") (text . "") (js . "$(\"p:even\").removeClass(\"blue\");") (text . "") (css . "

			      p { margin: 4px; font-size:16px; font-weight:bolder; }
			      .blue { color:blue; }
			      .under { text-decoration:underline; }
			      .highlight { background:yellow; }
			      ") (text . "") (html . "<p class=\"blue under\">Hello</p>
			      <p class=\"blue under highlight\">and</p>
			      <p class=\"blue under\">then</p>

			      <p class=\"blue under\">Goodbye</p>") (text . "")) ((text . "") (text . "Remove the class ʼblueʼ and ʼunderʼ from the matched elements.

") (text . "") (js . "$(\"p:odd\").removeClass(\"blue under\");") (text . "") (css . "
			      p { margin: 4px; font-size:16px; font-weight:bolder; }
			      .blue { color:blue; }
			      .under { text-decoration:underline; }
			      .highlight { background:yellow; }
			      ") (text . "") (html . "<p class=\"blue under\">Hello</p>

			      <p class=\"blue under highlight\">and</p>
			      <p class=\"blue under\">then</p>
			      <p class=\"blue under\">Goodbye</p>") (text . "")) ((text . "") (text . "Remove all the classes from the matched elements.

") (text . "") (js . "$(\"p:eq(1)\").removeClass();") (text . "") (css . "

			      p { margin: 4px; font-size:16px; font-weight:bolder; }
			      .blue { color:blue; }
			      .under { text-decoration:underline; }
			      .highlight { background:yellow; }
			      ") (text . "") (html . "<p class=\"blue under\">Hello</p>
			      <p class=\"blue under highlight\">and</p>
			      <p class=\"blue under\">then</p>

			      <p class=\"blue under\">Goodbye</p>") (text . ""))))) jquery-doc-hash)

(push "hasClass" jquery-methods)

(puthash "hasClass" (quote (("name" . "hasClass") ("signatures" "hasClass" (("className" "The class name to search for.

" nil nil))) ("desc" (text . "Determine whether any of the matched elements are assigned the given
class.

")) ("longdesc" (text . "Elements may have more than one class assigned to them. In HTML, this
is represented by separating the class names with a space:

") (text . "") (js . "<div id=\"mydiv\" class=\"foo bar\"></div>") (text . "") (text . "The .hasClass() method will return true if the class is assigned to an
element, even if other classes also are. For example, given the HTML
above, the following will return true:
") (text . "") (js . "$('#mydiv').hasClass('foo')") (text . "") (text . "As would:


") (text . "") (js . "$('#mydiv').hasClass('bar')") (text . "") (text . "While this would return false:


") (text . "") (js . "$('#mydiv').hasClass('quux')")) ("examples" ((text . "") (text . "Looks for the paragraph that contains ʼselectedʼ as a class.

") (text . "") (js . "
			      $(\"div#result1\").append($(\"p:first\").hasClass(\"selected\").toString());
			      $(\"div#result2\").append($(\"p:last\").hasClass(\"selected\").toString());
			      $(\"div#result3\").append($(\"p\").hasClass(\"selected\").toString());
			      ") (text . "") (css . "
			      p { margin: 8px; font-size:16px; }
			      .selected { color:red; }
			      ") (text . "") (html . "
			      <p>This paragraph is black and is the first paragraph.</p>
			      <p class=\"selected\">This paragraph is red and is the second paragraph.</p>
			      
			      <div id=\"result1\">First paragraph has selected class: </div>
			      <div id=\"result2\">Second paragraph has selected class: </div>
			      <div id=\"result3\">At least one paragraph has selected class: </div>") (text . ""))))) jquery-doc-hash)

(push "removeAttr" jquery-methods)

(puthash "removeAttr" (quote (("name" . "removeAttr") ("signatures" "removeAttr" (("attributeName" "An attribute to remove.

" nil nil))) ("desc" (text . "Remove an attribute from each element in the set of matched elements.

")) ("longdesc" (text . "The .removeAttr() method uses the JavaScript removeAttribute()
function, but it has the advantage of being able to be called directly
on a jQuery object and it accounts for different attribute naming
across browsers.
")) ("examples" ((text . "") (text . "Clicking the button enables the input next to it.

") (text . "") (js . "
			      $(\"button\").click(function () {
			      $(this).next().removeAttr(\"disabled\")
			      .focus()
			      .val(\"editable now\");
			      });
			      ") (text . "") (html . "<button>Enable</button>
			      <input type=\"text\" disabled=\"disabled\" value=\"can't edit this\" />") (text . ""))))) jquery-doc-hash)

(push "attr" jquery-methods)

(puthash "attr" (quote (("name" . "attr") ("signatures" "attr" (("attributeName" "The name of the attribute to get.

" nil nil))) ("desc" (text . "Get the value of an attribute for the first element in the set of
matched elements.

")) ("longdesc" (text . "The .attr() method gets the attribute value for only the first element
in the matched set. To get the value for each element individually, use
a looping construct such as jQueryʼs .each() or .map() method.
") (text . "") (text . "As of jQuery 1.6, the .attr() method returns undefined for attributes
that have not been set. In addition, .attr() should not be used on
plain objects, arrays, the window, or the document. To retrieve and
change DOM properties, use the .prop() method.
") (text . "") (text . "Using jQueryʼs .attr() method to get the value of an elementʼs
attribute has two main benefits:

") (text . "") (text . " 1. Convenience: It can be called directly on a jQuery object and
    chained to other jQuery methods.
 2. Cross-browser consistency: The values of some attributes are
    reported inconsistently across browsers, and even across versions
    of a single browser. The .attr() method reduces such
    inconsistencies.
") (text . "")) ("examples" ((text . "") (text . "Find the title attribute of the first <em> in the page.

") (text . "") (js . "
			      var title = $(\"em\").attr(\"title\");
			      $(\"div\").text(title);
			      ") (text . "") (css . "
			      em { color:blue; font-weight;bold; }
			      div { color:red; }
			      ") (text . "") (html . "
			      <p>
			      Once there was a <em title=\"huge, gigantic\">large</em> dinosaur...
			      </p>

			      The title of the emphasis is:<div></div>
			      ") (text . ""))))) jquery-doc-hash)

(push "attr" jquery-methods)

(puthash "attr" (quote (("name" . "attr") ("signatures" "attr" (("attributeName" "The name of the attribute to set.

" nil nil) ("value" "A value to set for the attribute.

" nil nil)) (("map" "A map of attribute-value pairs to set.

" nil nil)) (("attributeName" "The name of the attribute to set.

" nil nil) ("function(index, attr)" "A function returning the value to set. this is the current element.
Receives the index position of the element in the set and the old
attribute value as arguments.
" nil nil))) ("desc" (text . "Set one or more attributes for the set of matched elements.

")) ("longdesc" (text . "The .attr() method is a convenient way to set the value of
attributesâespecially when setting multiple attributes or using values
returned by a function. Consider the following image:
") (text . "") (js . "<img id=\"greatphoto\" src=\"brush-seller.jpg\" alt=\"brush seller\" />") (text . "") (text . " Setting a simple attribute


") (text . "") (text . "To change the alt attribute, simply pass the name of the attribute and
its new value to the .attr() method:

") (text . "") (js . "$('#greatphoto').attr('alt', 'Beijing Brush Seller');") (text . "") (text . "Add an attribute the same way:


") (text . "") (js . "$('#greatphoto')
			    .attr('title', 'Photo by Kelly Clark');") (text . "") (text . " Setting several attributes at once


") (text . "") (text . "To change the alt attribute and add the title attribute at the same
time, pass both sets of names and values into the method at once using
a map (JavaScript object literal). Each key-value pair in the map adds
or modifies an attribute:
") (text . "") (js . "$('#greatphoto').attr({
			    alt: 'Beijing Brush Seller',
			    title: 'photo by Kelly Clark'
			    });") (text . "") (text . "When setting multiple attributes, the quotes around attribute names are
optional.

") (text . "") (text . "WARNING: When setting the ʼclassʼ attribute, you must always use
quotes!

") (text . "") (text . "Note: Internet Explorer does not allow you to change the type attribute
of an <input> or <button> element.

") (text . "") (text . " Computed attribute values


") (text . "") (text . "By using a function to set attributes, you can compute the value based
on other properties of the element. For example, to concatenate a new
value with an existing value:
") (text . "") (js . "$('#greatphoto').attr('title', function(i, val) {
			    return val + ' - photo by Kelly Clark'
			    });") (text . "") (text . "This use of a function to compute attribute values can be particularly
useful when modifying the attributes of multiple elements at once.

") (text . "Note: If nothing is returned in the setter function (ie.
function(index, attr){}), the current value of the attribute is
returned as it acts as a getter when the value is undefined.
Effectively, no changes are made if the setter function does not return
something.
")) ("examples" ((text . "") (text . "Set some attributes for all <img>s in the page.

") (text . "") (js . "
			      $(\"img\").attr({ 
			      src: \"/images/hat.gif\",
			      title: \"jQuery\",
			      alt: \"jQuery Logo\"
			      });
			      $(\"div\").text($(\"img\").attr(\"alt\"));
			      ") (text . "") (css . "
			      img { padding:10px; }
			      div { color:red; font-size:24px; }
			      ") (text . "") (html . "
			      <img />
			      <img />
			      <img />
			      
			      <div><B>Attribute of Ajax</B></div>
			      ") (text . "")) ((text . "") (text . "Disable buttons greater than the 1st button.

") (text . "") (js . "
			      $(\"button:gt(1)\").attr(\"disabled\",\"disabled\");
			      ") (text . "") (css . "
			      button { margin:10px; }
			      ") (text . "") (html . "
			      <button>0th Button</button>
			      <button>1st Button</button>
			      <button>2nd Button</button>
			      ") (text . "")) ((text . "") (text . "Set the id for divs based on the position in the page.

") (text . "") (js . "
			      $(\"div\").attr(\"id\", function (arr) {
			      return \"div-id\" + arr;
			      })
			      .each(function () {
			      $(\"span\", this).html(\"(ID = '<b>\" + this.id + \"</b>')\");
			      });
			      ") (text . "") (css . "
			      div { color:blue; }
			      span { color:red; }
			      b { font-weight:bolder; }
			      ") (text . "") (html . "
			      <div>Zero-th <span></span></div>
			      <div>First <span></span></div>
			      <div>Second <span></span></div>
			      ") (text . "")) ((text . "") (text . "Set the src attribute from title attribute on the image.

") (text . "") (js . "
			      $(\"img\").attr(\"src\", function() { 
			      return \"/images/\" + this.title; 
			      });
			      ") (text . "") (html . "
			      <img title=\"hat.gif\"/>
			      ") (text . ""))))) jquery-doc-hash)

(push "addClass" jquery-methods)

(puthash "addClass" (quote (("name" . "addClass") ("signatures" "addClass" (("className" "One or more class names to be added to the class attribute of each
matched element.

" nil nil)) (("function(index, currentClass)" "A function returning one or more space-separated class names to be
added. Receives the index position of the element in the set and the
old class value as arguments.
" nil nil))) ("desc" (text . "Adds the specified class(es) to each of the set of matched elements.

")) ("longdesc" (text . "Itʼs important to note that this method does not replace a class. It
simply adds the class, appending it to any which may already be
assigned to the elements.
") (text . "") (text . "More than one class may be added at a time, separated by a space, to
the set of matched elements, like so:

") (text . "") (js . "$(\"p\").addClass(\"myClass yourClass\");") (text . "") (text . "This method is often used with .removeClass() to switch elementsʼ
classes from one to another, like so:

") (text . "") (js . "$(\"p\").removeClass(\"myClass noClass\").addClass(\"yourClass\");") (text . "") (text . "Here, the myClass and noClass classes are removed from all paragraphs,
while yourClass is added.

") (text . "") (text . "As of jQuery 1.4, the .addClass() methodʼs argument can receive a
function.

") (text . "") (js . "$(\"ul li:last\").addClass(function() {
			    return \"item-\" + $(this).index();
			    });") (text . "") (text . "Given an unordered list with five <li> elements, this example adds the
class \"item-4\" to the last <li>.

") (text . "")) ("examples" ((text . "") (text . "Adds the class \"selected\" to the matched elements.

") (text . "") (js . "
			      $(\"p:last\").addClass(\"selected\");
			      ") (text . "") (css . "
			      p { margin: 8px; font-size:16px; }
			      .selected { color:blue; }
			      .highlight { background:yellow; }
			      ") (text . "") (html . "
			      <p>Hello</p>
			      <p>and</p>
			      <p>Goodbye</p>
			      ") (text . "")) ((text . "") (text . "Adds the classes \"selected\" and \"highlight\" to the matched elements.

") (text . "") (js . "
			      $(\"p:last\").addClass(\"selected highlight\");
			      ") (text . "") (css . "
			      p { margin: 8px; font-size:16px; }
			      .selected { color:red; }
			      .highlight { background:yellow; }
			      ") (text . "") (html . "<p>Hello</p>
			      <p>and</p>
			      <p>Goodbye</p>") (text . "")) ((text . "") (text . "Pass in a function to .addClass() to add the \"green\" class to a div
that already has a \"red\" class.

") (text . "") (js . "
			      $(\"div\").addClass(function(index, currentClass) {
			      var addedClass;

			      if ( currentClass === \"red\" ) {
			      addedClass = \"green\";
			      $(\"p\").text(\"There is one green div\");
			      }
			      
			      return addedClass;
			      });
			      ") (text . "") (css . "
			      div { background: white; }
			      .red { background: red; }
			      .red.green { background: green; }
			      ") (text . "") (html . "
			      <div>This div should be white</div>
			      <div class=\"red\">This div will be green because it now has the \"green\" class.
			      It would be red if the addClass function failed.</div>
			      <div>This div should be white</div>
			      <p>There are zero green divs</p>
			      ") (text . ""))))) jquery-doc-hash)

(push "$.tmpl" jquery-methods)

(puthash "$.tmpl" (quote (("name" . "$.tmpl") ("signatures" "$.tmpl" (("template" "The HTML markup or text to use as a template.

" nil nil) ("data" "The data to render. This can be any JavaScript type, including Array or
Object.

" "true" nil) ("options" "An optional map of user-defined key-value pairs. Extends the tmplItem
data structure, available to the template during rendering.

" "true" nil))) ("desc" (text . "Render the specified HTML content as a template, using the specified
data.

")) ("longdesc" (text . "") (text . "The jQuery.tmpl() method is designed for chaining with .appendTo,
.prependTo, .insertAfter or .insertBefore as in the following example.

") (text . "") (text . "Example:


") (text . "") (js . "$.tmpl( \"<li>${Name}</li>\", { \"Name\" : \"John Doe\" }).appendTo( \"#target\" );") (text . "") (text . "The template parameter can be any of the following:


") (text . "") (text . "  * A string containing markup.
  * An HTML element (or jQuery object that wraps an element) whose
    content is to be used as the template.
  * A string corresponding to the name of a named template (see
    jQuery.template() and .template()).
  * A compiled-template function (see jQuery.template() and
    .template()).
") (text . "") (text . "If data is an array, the template is rendered once for each data item
in the array. If data is an object, or if the data parameter is missing
or null, a single template item is rendered.
") (text . "") (text . "The return value is a jQuery collection of elements made up of the
rendered template items (one for each data item in the array). If the
template contains only one top-level element, then there will be one
element for each data item in the array.
") (text . "") (text . "To insert the rendered template items into the HTML DOM, the returned
jQuery collection should not be inserted directly into the DOM, but
should be chained with .appendTo, .prependTo, .insertAfter or
.insertBefore, as in following example:
") (text . "") (js . "$.tmpl( myTemplate, myData ).appendTo( \"#target\" );") (text . "") (text . "See also .tmpl().


") (text . "") (text . " Example


") (text . "") (text . "The following example shows how to use jQuery.tmpl() to render local
data, using a template provided as a string:

") (text . "") (js . "<ul id=\"movieList\"></ul>
				
				<script type=\"text/javascript\">
				var movies = [
				{ Name: \"The Red Violin\", ReleaseYear: \"1998\" },
				{ Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\" },
				{ Name: \"The Inheritance\", ReleaseYear: \"1976\" }
				];
				
				var markup = \"<li><b>${Name}</b> (${ReleaseYear})</li>\";
				
				// Compile the markup as a named template
				$.template( \"movieTemplate\", markup );
				
				// Render the template with the movies data and insert
				// the rendered HTML under the \"movieList\" element
				$.tmpl( \"movieTemplate\", movies )
				.appendTo( \"#movieList\" );
				</script>
				") (text . "") (text . " Using Remote Data


") (text . "") (text . "Typically the data is not local and is instead obtained using an Ajax
request to a remote service or page, as in the following example:

") (text . "") (js . "var markup = \"<li><b>${Name}</b> (${ReleaseYear})</li>\";
				
				// Compile the markup as a named template
				$.template( \"movieTemplate\", markup );
				
				$.ajax({
				dataType: \"jsonp\",
				url: moviesServiceUrl,
				jsonp: \"$callback\",
				success: showMovies
				});
				
				// Within the callback, use .tmpl() to render the data.
				function showMovies( data ) {
				// Render the template with the \"movies\" data and insert
				// the rendered HTML under the 'movieList' element
				$.tmpl( \"movieTemplate\", data )
				.appendTo( \"#movieList\" );
				}
				") (text . "") (text . " The Markup for the Template


") (text . "") (text . "You can get the markup for the template from inline markup in the page,
or from a string (possibly computed, or obtained remotely). For an
example of how to use inline markup, see .tmpl().
") (text . "") (text . " Caching the Template


") (text . "") (text . "When a template is rendered, the markup is first converted into a
compiled-template function. Every time $.tmpl( markup , myData
).appendTo( \"#target\" ) is called, the template is recompiled. If the
same template is to be used more than once for rendering data, you
should ensure that the compiled template is cached. To cache the
template when using markup that is obtained from a string (rather than
from inline markup in the page), use $.template( name, markup ) to
create a named template for reuse. See jQuery.template().
") (text . "") (text . " Template Tags, Expressions, and Template Variables


") (text . "") (text . "Template tags such as the ${} tag can used within jQuery templates in
addition to text and HTML markup to enable a number of scenarios such
as composition of templates, iteration over hierarchical data,
parameterization of template rendering, etc. Template tags can render
content based on the values of data item fields or template variables
such as $item (corresponding to the template item), as well as
expressions and function calls. See the documentation topics for each
template tag: ${}, {{each}}, {{if}}, {{else}}, {{html}}, {{tmpl}} and
{{wrap}}.
") (text . "") (text . " The options Parameter, and Template Items


") (text . "") (text . "Each template item (the result of rendering a data item with the
template) is associated with a tmplItem data structure, which can be
accessed using jQuery.tmplItem() and .tmplItem(), or the $item template
variable. Any fields or anonomyous methods passed in with the options
parameter of jQuery.tmpl() will extend the tmplItem data structure, and
so be available to the template as in the following example:
") (text . "") (js . "
				  var markup = \"<li>Some content: ${$item.myMethod()}.<br/>\" 
				  + \" More content: ${$item.myValue}.</li>\";

				  // Compile the markup as a named template
				  $.template( \"movieTemplate\", markup );

				  // Render the template with the movies data
				  $.tmpl( \"movieTemplate\", movies,
				  { 
				  myValue: \"somevalue\", 
				  myMethod: function() { 
				  return \"something\";
				  } 
				  } 
				  ).appendTo( \"#movieList\" );
				") (text . "")) ("examples" ((text . "") (text . "Render local data using jQuery.tmpl().

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
				var movies = [
				{ Name: \"The Red Violin\", ReleaseYear: \"1998\" },
				{ Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\" },
				{ Name: \"The Inheritance\", ReleaseYear: \"1976\" }
				];

				var markup = \"<li><b>${Name}</b> (${ReleaseYear})</li>\";

				/* Compile the markup as a named template */
				$.template( \"movieTemplate\", markup );

				/* Render the template with the movies data and insert
				the rendered HTML under the \"movieList\" element */
				$.tmpl( \"movieTemplate\", movies )
				.appendTo( \"#movieList\" );
				") (text . "") (html . "
				<ul id=\"movieList\"></ul>
				") (text . "")) ((text . "") (text . "Render data from a remote service, using jQuery.tmpl().

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
				var markup = \"<li><b>${Name}</b> (${ReleaseYear})</li>\";

				/* Compile the markup as a named template */
				$.template( \"movieTemplate\", markup );

				function getMovies( genre, skip, top ) {
				$.ajax({
				dataType: \"jsonp\",
				url: \"http://odata.netflix.com/Catalog/Genres('\" + genre
				+ \"')/Titles?$format=json&$skip=\"
				+ skip + \"&$top=\" + top,
				jsonp: \"$callback\",
				success: function( data ) {
				/* Get the movies array from the data */
				var movies = data.d;

				/* Remove current set of movie template items */
				$( \"#movieList\" ).empty();

				/* Render the template items for each movie
				and insert the template items into the \"movieList\" */
				$.tmpl( \"movieTemplate\", movies )
				.appendTo( \"#movieList\" );
				}
				});
				}

				$( \"#cartoonsBtn\" ).click( function() {
				getMovies( \"Cartoons\", 0, 6 );
				});

				$( \"#dramaBtn\" ).click( function() {
				getMovies( \"Drama\", 0, 6 );
				});

				") (text . "") (html . "
				<button id=\"cartoonsBtn\">Cartoons</button>
				<button id=\"dramaBtn\">Drama</button>

				<ul id=\"movieList\"></ul>
				") (text . ""))))) jquery-doc-hash)

(push "tmplItem" jquery-methods)

(puthash "tmplItem" (quote (("name" . "tmplItem") ("signatures" "tmplItem" nil) ("desc" (text . "Return the tmplItem data structure for the rendered template that the
matched element is part of.

")) ("longdesc" (text . "") (text . "Note: For information about how to render templates, see .tmpl() and
jQuery.tmpl().

") (text . "") (text . "$(selector).tmplItem() provides access to the rendered template item
which the target element of the selector is part of.

") (text . "") (text . "See also jQuery.tmplItem().


") (text . "") (text . "The return value of tmplItem() is a tmplItem data structure whose
fields provide access to:

") (text . "") (text . "  * The HTML elements that the template item is made up of ( nodes
    field).
  * The associated data item ( data field).
  * The parent template item, if the template is nested ( parent
    field).
  * The template that was used to render the template item ( tmpl
    field).
  * User defined parameters or methods, such as any values that were
    set on the options map, passed to tmpl() when the template was
    rendered.
") (text . "") (text . "The following example shows how to use .tmplItem() to get information
about the rendered template:

") (text . "") (js . "
					var tmplItem = $( selector ).tmplItem();
					alert( \"Description: \" + tmplItem.data.description );
				      ") (text . "") (text . " Building Interactive Ajax Applications


") (text . "") (text . ".tmplItem() and jQuery.tmplItem() make it easy to use templates in
scenarios beyond simple string concatenation and read-only rendering.
They let you create fully-fledged interactive client-side Ajax
applications in which the code needs to perform actions like the
following:
") (text . "") (text . "  * Accessing the associated data item.
  * Modifying the data item.
  * Accessing HTML elements that make up the rendered template item.
  * Updating (re-rendering) the template item, with modified data,
    modified user-defined parameters, or using a different template
") (text . "") (text . "Example: Access data and HTML elements for a template item.:


") (text . "") (js . "
					// Get the template item for an element
					var tmplItem = $( selector ).tmplItem();
					
					// Get the corresponding data item and HTML nodes
					var movieData = tmplItem.data;
					var htmlNodes = tmplItem.nodes;
					
					// Modify style
					$( htmlNodes ).css( \"backgroundColor\", color );
					
					// Access data
					alert( \"'\" + movieData.Name + \"' was released in \"
					+ movieData.ReleaseYear + \".\" );
				      ") (text . "") (text . "The following example is from the Master Detail sample, below. It uses
.tmplItem() to set selection on the new item that is added to the list:

") (text . "") (js . "
					$(\"#addBtn\").click( function () {
					// Add a new item to the data array
					people.push( { firstName: \"first\", lastName: \"last\" } );
					
					// Render the template with the new data
					renderTemplate( \"#peopleList\", \"#listItemTemplate\", people );
					
					// Find the added template item
					var addedTmplItem = $(\"#peopleList tr:last\").tmplItem();
    					
					// Set selection on the added item
					select ( addedTmplItem );
					});
				      ") (text . "")) ("examples" ((text . "") (text . "Access the template item of matched element, to show data and to modify
CSS styles on the rendered template.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
				      var movies = [ 
				      { Name: \"The Red Violin\", ReleaseYear: \"1998\" },
				      { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\" },
				      { Name: \"The Inheritance\", ReleaseYear: \"1976\" }
				      ];
				      var color = \"aqua\"; 
				      /* Render the template with the movies data */
				      $( \"#movieTemplate\" ).tmpl( movies )
				      .appendTo( \"#movieList\" );
				      
				      $( \"#lastItemBtn\" ).click( function() {
				      /* Flip the color */
				      color = (color === \"aqua\" ? \"yellow\" : \"aqua\");
				      
				      /* Get the data structure for the last
				      template item in the list */
				      var lastTmplItem = $( \"li:last\" ).tmplItem();
				      
				      /* Get the corresponding data item and HTML nodes */
				      var movie = lastTmplItem.data;
				      var htmlNodes = lastTmplItem.nodes;
				      
				      /* Switch the background color */
				      $( htmlNodes ).css( \"backgroundColor\", color );
				      
				      /* Acces the data */
				      alert( \"'\" + movie.Name + \"' was released in \" + movie.ReleaseYear + \".\" );
				      });
				      ") (text . "") (html . "
				      <tmpl id=\"movieTemplate\" type=\"text/x-jquery-tmpl\">
				      <li>
				      <b>${Name}</b>
				      </li>
				      </tmpl>
				      
				      <button id=\"lastItemBtn\">Details of last movie</button>
				      
				      <ul id=\"movieList\"></ul>
				      ") (text . "")) ((text . "") (text . "Editable master detail view.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
				      var people = [
				      { firstName: \"Peter\", lastName: \"Jones\" },
				      { firstName: \"Eva\", lastName: \"Smolinski\" }
				      ];
				      
				      var selectedItem = null;
				      
				      function renderTemplate( container, template, data ) {
				      $( container ).empty();
				      $( template ).tmpl( data ).appendTo( container );
				      }
				      
				      /* Render the list */
				      renderTemplate( \"#peopleList\", \"#listItemTemplate\", people );
				      
				      function select( tmplItem ) {
				      if ( selectedItem ) {
				      $( selectedItem.nodes ).removeClass( \"selected\");
				      }
				      $( tmplItem.nodes ).addClass( \"selected\");
				      selectedItem = tmplItem;
				      
				      /* Render the detail view for this data item */
				      renderTemplate( \"#personDetail\", \"#detailTemplate\", tmplItem.data );
				      }
				      
				      $(\"#addBtn\").click( function () {
				      /* Add a new item to the data array */
				      people.push( { firstName: \"first\", lastName: \"last\" } );
				      
				      /* Render the template with the new data */
				      renderTemplate( \"#peopleList\", \"#listItemTemplate\", people );
				      
				      /* Find the added template item */
				      var addedTmplItem = $(\"#peopleList tr:last\").tmplItem();
    				      
				      /* Set selection on the added item */
				      select ( addedTmplItem );
				      });
				      
				      $(\"#peopleList\")
				      .delegate( \"tr\", \"click\", function () {
				      /* Set selection on the clicked item */
				      select ( $.tmplItem(this) );
				      });
				      
				      $(\"#personDetail\")
				      .delegate( \"input\", \"change\", function () {
				      /* Set the data to the modified value */
				      $.tmplItem(this).data[ this.name ] = this.value;
				      
				      /* Render the list, to show the updated data */ 
				      renderTemplate( \"#peopleList\", \"#listItemTemplate\", people );
				      });
				      ") (text . "") (css . "
				      table {cursor:pointer;border-collapse:collapse;float:left;clear:both;} 
				      table tr {border:1px solid blue;color:blue;height:25px;} 
				      table tr:hover {color:red;}
				      table, #personDetail > div {border:2px solid blue;width:230px;
				      margin:4px 0 4px 4px;
				      background-color:#f8f8f8;} 
				      table td, #personDetail div div {padding:3px;margin:3px;}
				      .selected {background-color:yellow;} 
				      #personDetail input {float:right;width:125px;} 
				      #personDetail {float:left;margin-left:10px;} 
				      button {float:left;margin:4px;}
				      ") (text . "") (html . "
				      <tmpl id=\"listItemTemplate\" type=\"text/x-jquery-tmpl\"> 
				      <tr><td>
				      ${firstName} ${lastName} 
				      </td></tr>
				      </tmpl>
				      
				      <tmpl id=\"detailTemplate\" type=\"text/x-jquery-tmpl\"> 
				      <div>
				      <div>First Name: <input name=\"firstName\" value=\"${firstName}\"/></div>
				      <div>Last Name: <input name=\"lastName\" value=\"${lastName}\"/></div>
				      </div>
				      </tmpl>
				      
				      <button id=\"addBtn\">Add a person</button>
				      
				      <table><tbody id=\"peopleList\"></tbody></table>
				      
				      <div id=\"personDetail\"></div>
				      ") (text . ""))))) jquery-doc-hash)

(push "template" jquery-methods)

(puthash "template" (quote (("name" . "template") ("signatures" "template" (("name" "A string naming the compiled template.

" "true" nil))) ("desc" (text . "Compile the contents of the matched element as a reusable compiled
template.

")) ("longdesc" (text . "") (text . "Note: For information about how to render templates, see .tmpl() and
jQuery.tmpl().

") (text . "") (text . "This method returns a compiled template, created from the content of
the first matched element. If the name parameter is provided the
compiled template is stored as a named template, and can be referenced
using the specified string.
") (text . "") (text . "See also jQuery.template().


") (text . "") (text . "Example: Create a compiled template associated with the name
\"summaryTemplate\" and then reference it by name for rendering:

") (text . "") (js . "
      <script id=\"titleTemplate\" type=\"text/x-jquery-tmpl\">
      <li>${Name}</li>
      </script>
      ___________
      
      // Compile the inline template as a named template
      $( \"#titleTemplate\" ).template( \"summaryTemplate\" );
      
      function renderList() {
      // Render the movies data using the named template: \"summaryTemplate\"
      $.tmpl( \"summaryTemplate\", movies ).appendTo( \"#moviesList\" );
      }
    ") (text . "") (text . "Example: Use the return value rather than the name string to reference
the compiled template:

") (text . "") (js . "
      <script id=\"titleTemplate\" type=\"text/x-jquery-tmpl\">
      <li>${Name}</li>
      </script>
      ___________
      
      // Compile the inline template as a named template
      var myTemplate = $( \"#titleTemplate\" ).template();
      
      function renderList() {
      // Render movies data using the compiled template: myTemplate
      $.tmpl( myTemplate, movies ).appendTo( \"#moviesList\" );
      }
    ") (text . "") (text . "Example: Create a named template and reference it by name as a nested
template:

") (text . "") (js . "
      <script id=\"movieTemplate\" type=\"text/x-jquery-tmpl\">
      {{tmpl \"summaryTemplate\"}}
      <tr><td>Director: ${Director}</td></tr>
      </script>
      
      <script id=\"titleTemplate\" type=\"text/x-jquery-tmpl\">
      <tr><td>${Name}</td></tr>
      </script>
      ___________
      
      // Compile the titleTemplate template as a named template
      // referenced by the {{tmpl}} tag
      $( \"#titleTemplate\" ).template( \"summaryTemplate\" );
      
      // Render the movies data using the named template as a nested template
      $( \"#movieTemplate\" ).tmpl( movies ).appendTo( \"#movieList\" );
    ") (text . "") (text . "Example: Switch the template item to a different template, using
.template() to obtain compiled template:

") (text . "") (js . "
      <script id=\"summaryTemplate\" type=\"text/x-jquery-tmpl\">
      <tr>...</tr>
      </script>
      
      <script id=\"detailTemplate\" type=\"text/x-jquery-tmpl\">
      <tr>...</tr>
      </script>
      ___________
      
      // Render the summaryTemplate with the \"movies\" data 
      $( \"#summaryTemplate\" ).tmpl( movies ).appendTo( \"#movieList\" );
      
      $( \"tr\" ).click( function () {
      // Switch the template for this template item to
      // a different named template, then update the rendered item
      var tmplItem = $.tmplItem(this);
      tmplItem.tmpl = $( \"#detailTemplate\" ).template();
      tmplItem.update();
      });
    ") (text . "")) ("examples" ((text . "") (text . "Dynamic switching of templates, using .template() to obtain compiled
template.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
    var movies = [
    { Name: \"The Red Violin\", ReleaseYear: \"1998\", Director: \"François Girard\" },
    { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\", Director: \"Stanley Kubrick\" },
    { Name: \"The Inheritance\", ReleaseYear: \"1976\", Director: \"Mauro Bolognini\" }
    ];
    var selectedItem = null;
    
    /* Render the summaryTemplate with the \"movies\" data */ 
    $( \"#summaryTemplate\" ).tmpl( movies ).appendTo( \"#movieList\" );
    
    $(\"#movieList\")
    .delegate( \".movieSummary\", \"click\", function () {
    if (selectedItem) {
    /* Switch previously selected item back to the summaryTemplate */ 
    selectedItem.tmpl = $( \"#summaryTemplate\" ).template();
    
    /* Update rendering of previous selected item */ 
    selectedItem.update();
    }
    
    /* Make this the selected item  */
    selectedItem = $.tmplItem(this);
    
    /* Switch this template item to the detailTemplate */ 
    selectedItem.tmpl = $( \"#detailTemplate\" ).template();
    
    /* Refresh rendering */ 
    selectedItem.update();
    })
    .delegate( \".movieDetail\", \"click\", function () {
    /* Unselect - switch to the summaryTemplate */ 
    selectedItem.tmpl = $( \"#summaryTemplate\" ).template();
    
    /* Refresh rendering */ 
    selectedItem.update();
    
    selectedItem = null;
    });
    ") (text . "") (css . "
    table { cursor:pointer; border-collapse:collapse; border:2px solid blue; width:300px; margin:8px; }
    table tr { border:1px solid blue; color:blue; background-color:#f8f8f8; } 
    table td { padding:3px; } table tr:hover { color:red; }
    .movieDetail { background-color:yellow; } 
    .movieDetail.row1 { border-bottom:none; } .movieDetail.row2 { border-top:none; }
    ") (text . "") (html . "
    <tmpl id=\"summaryTemplate\" type=\"text/x-jquery-tmpl\">
    <tr class='movieSummary'><td colspan='2'>${Name}</td></tr>
    </tmpl>
    
    <tmpl id=\"detailTemplate\" type=\"text/x-jquery-tmpl\">
    <tr class='movieDetail row1'><td colspan='2'>${Name}</td></tr><tr class='movieDetail row2'><td>${ReleaseYear}</td><td>Director: ${Director}</td></tr>
    </tmpl>
    
    Click for details:
    <table><tbody id=\"movieList\"></tbody></table>
    ") (text . ""))))) jquery-doc-hash)

(push "unlink" jquery-methods)

(puthash "unlink" (quote (("name" . "unlink") ("signatures" "unlink" (("target" "An object to unlink.

" nil nil))) ("desc" (text . "Remove a previously created link.

")) ("longdesc" (text . "Links created with .link() can be removed with .unlink().


") (text . "") (js . "
	  var person = {};
	  $(\"form\").link(person);
	  $(\"[name=firstName]\").val(\"aValue\");
	  person.firstName; // aValue
	  $(\"form\").unlink(person);
	  $(\"[name=firstName]\").val(\"aNewValue\");
	  person.firstName; // still \"aValue\"
	") (text . "") (text . "If the original link matched multiple elements, .unlink() may also be
used to remove the link on a subset of the elements. The following
example shows how to link all input elements to an object, and then how
to unlink input elements that have a specified CSS class:
") (text . "") (js . "
	  var person = {};
	  $(\"input\").link(person);
	  $(\".nolink\").unlink(person);
	") (text . "") (text . ".unlink() can unlink elements that were part of the original link, but
note that .link() also responds to bubbled-up change events from the
selected elementsʼ descendants. .unlink() will only unlink elements
that were explicitly matched by the original link, not descendants of
those elements.
") (text . "")) ("examples" ((text . "") (text . "Link all input elements of a form to an object, then remove the link.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
	  var person = { };
	  $(\"form\").link(person);
	  
	  // Chain link the person object to these elements to show the results
	  $(\"#objFirst\").link(person, {
          firstName: {
          name: \"objFirst\",
          convertBack: function(value, source, target) {
          $(target).text(value);
          }
          }
	  });
	  $(\"#objLast\").link(person, {
          lastName: {
          name: \"objLast\",
          convertBack: function(value, source, target) {
          $(target).text(value);
          }
          }
	  });
	  
	  // remove link
	  $(\"#unlink\").click(function() {
          $(\"form\").unlink(person);
	  });
	  ") (text . "") (html . "
	  <form>
	  <div>
          First Name:
          <input type=\"text\" name=\"firstName\" />
	  </div>
	  <div>
          Last Name:
          <input type=\"text\" name=\"lastName\" />
	  </div>
	  <div>
          <input id=\"unlink\" type=\"button\" value=\"unlink\" />
	  </div>
	  </form>
	  Object.firstName: <span id=\"objFirst\"></span><br/>
	  Object.lastName <span id=\"objLast\"></span>
	  ") (text . ""))))) jquery-doc-hash)

(push "link" jquery-methods)

(puthash "link" (quote (("name" . "link") ("signatures" "link" (("target" "An object to link changes to.

" nil nil) ("settings" "A map describing the fields to link.

" "true" nil))) ("desc" (text . "Link changes to the matched elements to an object.

")) ("longdesc" (text . "The .link() method links form fields to an object. Any changes to the
form field values are automatically represented in the object. For
example, when an input element whose name attribute is \"firstName\" is
changed, the firstName property of the target is set to the new value:
") (text . "") (js . "
	  var person = {};
	  $(\"form\").link(person);
	  $(\"[name=firstName]\").val(\"NewValue\"); // Set firstName to a value.
	  person.firstName; // NewValue
	  
	  // User types a value into the form field.
	  person.firstName; // firstName now contains the user-ented value.
	") (text . "") (text . "By default, links are two-way, meaning changes to the object are also
automatically changed in the corresponding form field. For example, if
the firstName of the target is changed using the .setField() method,
the value of input element named \"firstName\" is set to the new value.
") (text . "") (js . "
	  // Set the object property.
	  $(person).setField(\"firstName\", \"NewValue\");
	  // The change is automatically pushed to the input element.
	  $(\"[name=firstName]\").val(); // The value is now \"NewValue\"
	") (text . "") (text . " Customizing the Mapping Between Elements and Objects


") (text . "") (text . "By default, all change events that occur for (or bubble up to) the
selected element or elements are processed, and the changes are made to
the property whose name corresponds to the name of the element that
changed. By providing a mapping object, you can explicitly choose which
elements participate in linking behavior, and what target properties
they set.
") (text . "") (js . "
	  var person = {};
	  $(\"form\").link(person, {
	  firstName: \"first-name\",
	  lastName: \"last-name\"
	  });
	") (text . "") (text . "The preceeding example shows how to link only the input element named
\"first-name\" to person.firstName, and the input element named
\"last-name\" to person.lastName. Changes in other input elements and
changes to other fields of the person object are ignored (by this
link).
") (text . "") (text . " Specifying One-Way Linking


") (text . "") (text . "You can disable two-way linking for individual fields with the twoWay
field in the custom mapping. The following example shows how to create
a link from the form field named \"firstName\" to the firstName property
of a person object, but not the other way around.
") (text . "") (js . "
	  var person = {};
	  $(\"form\").link(person, {
	  firstName: {
	  twoWay: false
	  }
	  });
	  $(person).setField(\"firstName\", \"NewValue\");
	  $(\"[name=firstName]\").val(); // unchanged
	") (text . "") (text . " Converting Values


") (text . "") (text . "By default, any changed value is assigned as-is to the target object.
Often times, it is necessary to modify the value, as converting null to
\"None\", formating or parsing a date, or parsing a string into a number.
To perform these conversions, you can specify a convert field in the
mapping that includes a conversion function, as shown in the following
example:
") (text . "") (js . "
	  var person = {};
	  $(\"[name=age]\")
	  .link(person, {
	  age: {
	  convert: function(value) {
	  return Math.round( parseFloat( value ) );
	  }
	  }
	  })
	  .val(\"7.5\");
	  person.age; // 8
	") (text . "") (text . "You can also define a converter using the $.convertFn object and refer
to it by name instead:

") (text . "") (js . "
	  var person = {};
	  $.convertFn.round = function(value) {
	  return Math.round( parseFloat( value ) );
	  }
	  $(\"[name=age]\")
	  .link(person, {
	  age: {
	  convert: \"round\"
	  }
	  })
	  .val(\"7.5\");
	  person.age; // 8
	") (text . "") (text . "You can customize the name in a custom mapping at the same time as a
converter by specifying the name. The following example creates a link
from the input element named \"first-name\" to the firstName property of
the person object using a converter named \"titleCase\".
") (text . "") (js . "
	  $(\"form\").link(person, {
	  firstName: {
	  name: \"first-name\"
	  convert: \"titleCase\"
	  }
	  });
	") (text . "") (text . "For two-way links, you can specify a converter for both directions
using convertBack setting:

") (text . "") (js . "
	  $(\"form\").link(obj, {
	  field: {
	  // converter1 and converter2 functions are defined elsewhere.
	  convert: converter1,
	  convertBack: converter2
	  }
	  });
	") (text . "") (text . "Converters receive the value and you can optionally also get the source
object and the target object as parameters. The source is where the
value comes from, and target is the object to set the value for. If the
converter returns a value, it is assigned to the target automatically.
") (text . "") (text . " Using Converters to Create Custom Linking Behavior


") (text . "") (text . "If the converter does not return a value or if it returns \"undefined\",
no automatic update occurs. You can use this feature to customize the
behavior of a link by updating the source and target objects in your
code directly and not returning a value. The following example uses a
converter that does not return a value, so no automatic update takes
place. Instead, the converter code explicitly updates properties of the
target object:
") (text . "") (js . "
	  var person = {};
	  $(\"[name=age]\").link(person, {
	  age: {
	  convert: function(value, source, target) {
	  var age = Math.round( parseFloat( value ) );
	  target.age = age;
	  target.canVote = age >= 18;
	  }
	  }
	  });
	  $(\"[name=age]\").val(\"7.5\");
	  person.age; // 8
	  person.canVote; // false
	  $(\"[name=age]\").val(\"18\");
	  person.canVote; // true
	") (text . "") (text . "You can also use this technique to establish links between any
available DOM elements. This following example links the age property
of the person object to the height of the target DOM element.
") (text . "") (js . "
	  
	  var person = {};
	  $(\"#ageDiv\").link(person, {
	  age: {
	  convertBack: function(value, source, target) {
	  $(target).height(parseFloat(value * 2));
	  }
	  }
	  });
	  $(person).setField(\"age\", 21);
	  $(\"#ageDiv\").height(); // 42
	") (text . "")) ("examples" ((text . "") (text . "Link all input elements of a form to an object.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
	var person = { };
	$(\"form\").link(person);
	
	// Chain link the person object to these elements to show the results
	$(\"#objFirst\").link(person, {
        firstName: {
        name: \"objFirst\",
        convertBack: function(value, source, target) {
        $(target).text(value);
        }
        }
	});
	$(\"#objLast\").link(person, {
        lastName: {
        name: \"objLast\",
        convertBack: function(value, source, target) {
        $(target).text(value);
        }
        }
	});
	") (text . "") (html . "
	<form>
	<div>
        First Name:
        <input type=\"text\" name=\"firstName\" />
	</div>
	<div>
        Last Name:
        <input type=\"text\" name=\"lastName\" />
	</div>
	</form>
	Object.firstName: <span id=\"objFirst\"></span><br/>
	Object.lastName: <span id=\"objLast\"></span>
	") (text . ""))))) jquery-doc-hash)

(push "$.tmplItem" jquery-methods)

(puthash "$.tmplItem" (quote (("name" . "$.tmplItem") ("signatures" "$.tmplItem" (("element" "An HTML element (or jQuery object that wraps an element)

" nil nil))) ("desc" (text . "Return the tmplItem data structure for the rendered template that the
specified element is part of.

")) ("longdesc" (text . "") (text . "Note: For information about how to render templates, see .tmpl() and
jQuery.tmpl().

") (text . "") (text . "$.tmplItem( element ) provides access to the rendered template item
which the element is part of.

") (text . "") (text . "See also tmplItem().


") (text . "") (text . "Typically the element parameter passed to tmplItem()is the this element
within an event handler. The return value of tmplItem() is a tmplItem
data structure whose fields provide access to:
") (text . "") (text . "  * The HTML elements that the template item is made up of ( nodes
    field).
  * The associated data item ( data field).
  * The parent template item, if the template is nested ( parent
    field).
  * The template that was used to render the template item ( tmpl
    field).
  * User defined parameters or methods, such as any values that were
    set on the options map, passed to tmpl() when the template was
    rendered.
") (text . "") (text . "The following example shows how to use $.tmplItem() to get information
about the rendered template:

") (text . "") (js . "
      function myClickHandler() {
      var tmplItem = $.tmplItem( this );
      alert( \"Description: \" + tmplItem.data.description );
      }
    ") (text . "") (text . " Building Interactive Ajax Applications


") (text . "") (text . ".tmplItem() and jQuery.tmplItem() make it easy to use templates in
scenarios beyond simple string concatenation and read-only rendering.
They let you create fully-fledged interactive client-side Ajax
applications in which the code needs to perform actions like the
following:
") (text . "") (text . "  * Accessing the associated data item.
  * Modifying the data item.
  * Accessing HTML elements that make up the rendered template item.
  * Updating (re-rendering) the template item, with modified data,
    modified user-defined parameters, or using a different template
") (text . "") (text . "Example: Dynamically switching templates for a template item.:


") (text . "") (js . "
      // Get the compiled detail template
      var detailTemplate = $( \"#detailTemplate\" ).template();
      
      // Add an onclick handler for template items currently 
      // using the summary template
      $(\".movieSummary\").live( \"click\", function () {
      // Get the data structure for the template item 
      // which this clicked element belongs to
      var tmplItem = $.tmplItem(this);
      
      // Set the template on this item to the detail template
      tmplItem.tmpl = detailTemplate;
      
      // re-render
      tmplItem.update();
      })
    ") (text . "")) ("examples" ((text . "") (text . "Access the data, and set selection on the item.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
    var movies = [
    { Name: \"The Red Violin\", ReleaseYear: \"1998\" },
    { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\" },
    { Name: \"The Inheritance\", ReleaseYear: \"1976\" }
    ];
    var selectedItem =  null;
    
    /* Render the template with the movies data */
    $( \"#movieTemplate\" ).tmpl( movies )
    .appendTo( \"#movieList\" );
    
    /* Add an onclick handler for the movie template items */
    $( \".movieName\" ).live( \"click\", function() {
    
    if ( selectedItem ) {
    $( selectedItem.nodes ).css( \"backgroundColor\", \"#f8f8f8\" );
    }
    
    /* Get the data structure for the template item 
    which this clicked element belongs to */
    selectedItem = $.tmplItem( this );
    
    $( selectedItem.nodes ).css( \"backgroundColor\", \"yellow\" );
    
    alert( \"'\" + selectedItem.data.Name + \"' was released in \" 
    + selectedItem.data.ReleaseYear + \".\" ); 
    });
    ") (text . "") (css . "
    #movieList { cursor:pointer; color:blue; margin:8px; background-color:#f8f8f8; }
    #movieList li:hover { color:red; }
    ") (text . "") (html . "
    <tmpl id=\"movieTemplate\" type=\"text/x-jquery-tmpl\"> 
    <li><b class=\"movieName\">${Name}</b></li>
    </tmpl>
    
    Click for details:
    <ul id=\"movieList\"></ul>
    ") (text . "")) ((text . "") (text . "Master detail view.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
    var people = [
    { firstName: \"Peter\", lastName: \"Jones\" },
    { firstName: \"Eva\", lastName: \"Smolinski\" }
    ];
    
    var selectedItem = null;
    
    function renderTemplate( container, template, data ) {
    $( container ).empty();
    $( template ).tmpl( data ).appendTo( container );
    }
    
    /* Render the list */
    renderTemplate( \"#peopleList\", \"#listItemTemplate\", people );
    
    $(\"#peopleList\")
    .delegate( \"tr\", \"click\", function () {
    
    if ( selectedItem ) {
    $( selectedItem.nodes ).removeClass( \"selected\");
    }
    
    /* Set selection on the clicked item */
    selectedItem = $.tmplItem(this);
    $( selectedItem.nodes ).addClass( \"selected\");
    
    /* Render the detail view for this data item */
    renderTemplate( \"#personDetail\", \"#detailTemplate\", selectedItem.data );
    });
    ") (text . "") (css . "
    table { cursor:pointer; border-collapse:collapse; float: left; clear: both; } table tr { border:1px solid blue; color:blue; height:25px; } table tr:hover { color:red; }
    table, #personDetail > div { border:2px solid blue; width:220px; margin:8px 0 4px 0; background-color:#f8f8f8; } table td, #personDetail div div { padding:3px; margin:3px; }
    .selected { background-color:yellow; } #personDetail input { float:right; width:125px; } #personDetail { float:left; margin-left:10px; }
    ") (text . "") (html . "
    <tmpl id=\"listItemTemplate\" type=\"text/x-jquery-tmpl\"> 
    <tr><td>
    ${firstName} ${lastName} 
    </td></tr>
    </tmpl>
    
    <tmpl id=\"detailTemplate\" type=\"text/x-jquery-tmpl\"> 
    <div>
    <div>First Name: <em>${firstName}</em></div>
    <div>Last Name: <em>${lastName}</em></div>
    </div>
    </tmpl>
    
    <div style=\"float:left;\">Click for details:<div>
    <table><tbody id=\"peopleList\"></tbody></table>
    
    <div id=\"personDetail\"></div>
    ") (text . "")) ((text . "") (text . "Dynamic switching of templates.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
    var movies = [
    { Name: \"The Red Violin\", ReleaseYear: \"1998\", Director: \"François Girard\" },
    { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\", Director: \"Stanley Kubrick\" },
    { Name: \"The Inheritance\", ReleaseYear: \"1976\", Director: \"Mauro Bolognini\" }
    ];
    var selectedItem = null;
    
    /* Render the summaryTemplate with the \"movies\" data */
    $( \"#summaryTemplate\" ).tmpl( movies ).appendTo( \"#movieList\" );
    
    /* Add onclick handlers for movie template items
    using the summary or details template */
    $(\"#movieList\")
    .delegate( \".movieSummary\", \"click\", function () {
    if (selectedItem) {
    // Set the template on the previously selected item
    // back to the summary template
    selectedItem.tmpl = $( \"#summaryTemplate\" ).template();
    selectedItem.update();
    }
    /* Get the data structure for the template item 
    which this clicked element belongs to, and make
    it the selected item */
    selectedItem = $.tmplItem(this);
    
    /* Set the template on this item to the detail template */
    selectedItem.tmpl = $( \"#detailTemplate\" ).template();
    selectedItem.update();
    })
    .delegate( \".movieDetail\", \"click\", function () {
    /* Set the template on this item to the summary template */
    selectedItem.tmpl = $( \"#summaryTemplate\" ).template();
    selectedItem.update();
    selectedItem = null;
    });
    ") (text . "") (css . "
    table { cursor:pointer; border-collapse:collapse; border:2px solid blue; width:300px; margin:8px; }
    table tr { border:1px solid blue; color:blue; background-color:#f8f8f8; } table td { padding:3px; } table tr:hover { color:red; }
    .movieDetail { background-color:yellow; } .movieDetail.row1 { border-bottom:none; } .movieDetail.row2 { border-top:none; }
    ") (text . "") (html . "
    <tmpl id=\"summaryTemplate\" type=\"text/x-jquery-tmpl\">
    <tr class='movieSummary'><td colspan='2'>${Name}</td></tr>
    </tmpl>
    
    <tmpl id=\"detailTemplate\" type=\"text/x-jquery-tmpl\">
    <tr class='movieDetail row1'><td colspan='2'>${Name}</td></tr><tr class='movieDetail row2'><td>${ReleaseYear}</td><td>Director: ${Director}</td></tr>
    </tmpl>
    
    Click for details:
    <table><tbody id=\"movieList\"></tbody></table>
    ") (text . ""))))) jquery-doc-hash)

(push "$.template" jquery-methods)

(puthash "$.template" (quote (("name" . "$.template") ("signatures" "$.template" (("name" "A string naming the compiled template.

" nil nil) ("template" "The HTML markup and/or text to be used as template. Can be a string, or
an HTML element (or jQuery object wrapping an element) whose content is
to be used as template
" nil nil))) ("desc" (text . "Create a reusable named template (compiled from markup).

")) ("longdesc" (text . "") (text . "Note: For information about how to render templates, see .tmpl() and
jQuery.tmpl().

") (text . "") (text . "This method compiles the markup in the template parameter as a named
template, which can be referenced using the string specified in the
name parameter.
") (text . "") (text . "The return value is the compiled-template function.


") (text . "") (text . "See also template().


") (text . "") (text . "  Note: The named template is added to the $.template map.
  * To determine if a string \"someName\" is the name of a named
    template, test whether $.template[\"someName\"] is defined.
  * To remove a previously created named template, use
    delete $.template[\"someName\"];
") (text . "") (text . "Example: Create a compiled template associated with the name
\"summaryTemplate\" and then reference it by name for rendering:

") (text . "") (js . "
	// Convert the markup string into a named template
	$.template( \"summaryTemplate\", \"<li>${Name}</li>\" );
	
	function renderList() {
	// Render the movies data using the named template: \"summaryTemplate\"
	$.tmpl( \"summaryTemplate\", movies ).appendTo( \"#moviesList\" );
	}
      ") (text . "") (text . "Example: Use the return value rather than the name string to reference
the compiled template:

") (text . "") (js . "
	// Convert the markup string into a compiled template
	var myTemplate = $.template( null, \"<li>${Name}</li>\" ); 
	
	function renderList() {
	// Render movies data using the compiled template: myTemplate
	$.tmpl( myTemplate, movies ).appendTo( \"#moviesList\" );
	}
      ") (text . "") (text . "Example: Create a named template and reference it by name as a nested
template:

") (text . "") (js . "
	<script id=\"movieTemplate\" type=\"text/x-jquery-tmpl\">
	{{tmpl \"summaryTemplate\"}}
	<tr><td>Director: ${Director}</td></tr>
	</script>
	___________
	
	// Convert the markup string into a named template,
	// referenced by the {{tmpl}} tag
	$.template( \"summaryTemplate\", \"<tr><td>${Name}</td></tr>\" );
	
	// Render the movies data, using the named template as a nested template
	$( \"#movieTemplate\" ).tmpl( movies ).appendTo( \"#movieList\" );
      ") (text . "") (text . " Optimizing Template Rendering


") (text . "") (text . "When a template is rendered, using .tmpl() or jQuery.tmpl(), the markup
is first converted into a compiled-template function. In the case of
markup obtained from a string, the use of .template() as in the above
examples ensures that the conversion from markup to a compiled-template
function only happens once.
") (text . "") (text . "On the other hand, passing a markup string template directly to .tmpl()
or to {{tmpl}} for rendering will not be optimal from a performance
point of view, since the markup will be re-compiled every time:
") (text . "") (js . "
	var markup = \"<li>${Name}</li>\";
	
	function renderList() {
	// Sub-optimal: the markup string will be
	// recompiled each time renderList is called
	$.tmpl( markup, movies ).appendTo( \"#moviesList\" );
	}
      ") (text . "") (text . "Note: In the case of inline templates declared within a script block,
caching occurs automatically, so the following example does correspond
to best practice:
") (text . "") (text . "Example: Rendering an inline template directly without compiling as a
named template.

") (text . "") (js . "
	<script id=\"summaryTemplate\" type=\"text/x-jquery-tmpl\">
	<li>${Name}</li>
	</script>
	___________
	
	function renderList() {
	// The template will be compiled only once,
	// so this is approach can be optimal.
	$( \"#summaryTemplate\" ).tmpl( movies ).appendTo( \"#moviesList\" );
	}
      ") (text . "")) ("examples" ((text . "") (text . "Render template obtained from a markup string.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
      var movies = [
      { Name: \"The Red Violin\", ReleaseYear: \"1998\", Director: \"François Girard\" },
      { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\", Director: \"Stanley Kubrick\" },
      { Name: \"The Inheritance\", ReleaseYear: \"1976\", Director: \"Mauro Bolognini\" }
      ];
      
      var markup = \"<tr><td colspan='2'>${Name}</td><td>Released: ${ReleaseYear}</td><td>Director: ${Director}</td></tr>\"
      
      /* Compile markup string as a named template */
      $.template( \"movieTemplate\", markup );
      
      /* Render the named template */
      $( \"#showBtn\" ).click( function() {
      $( \"#movieList\" ).empty();
      $.tmpl( \"movieTemplate\", movies ).appendTo( \"#movieList\" );
      });
      ") (text . "") (css . "
      table { border-collapse:collapse; margin:8px; background-color:#f8f8f8; }
      table td { border:1px solid blue; padding:3px; }
      ") (text . "") (html . "
      <button id=\"showBtn\">Show movies</button><br/>
      <table><tbody id=\"movieList\"></tbody></table>
      ") (text . "")) ((text . "") (text . "Switch between templates obtained from markup strings.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
      var movies = [
      { Name: \"The Red Violin\", ReleaseYear: \"1998\", Director: \"François Girard\" },
      { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\", Director: \"Stanley Kubrick\" },
      { Name: \"The Inheritance\", ReleaseYear: \"1976\", Director: \"Mauro Bolognini\" }
      ];
      
      function renderTemplate( container, template, data ) {
      $( container ).empty();
      $.tmpl( template, data ).appendTo( container );
      }
      
      /* Compile markup as named templates */
      $.template(
      \"titleTemplate\",
      \"<tr><td>${Name}</td></tr>\"
      );
      $.template(
      \"detailTemplate\",
      \"<tr><td colspan='2'>${Name}</td><td>Released: ${ReleaseYear}</td><td>Director: ${Director}</td></tr>\"
      );
      
      var details = false;
      
      $( \"#switchBtn\" ).click( function() {
      details = !details;
      $(this).text( details ? \"Show titles\" : \"Show full details\" );
      /* Render using the other named template */
      renderTemplate( \"#movieList\", (details ? \"detailTemplate\" : \"titleTemplate\"), movies );
      });
      
      renderTemplate( \"#movieList\", \"titleTemplate\", movies );
      ") (text . "") (css . "
      table { border-collapse:collapse; margin:8px; background-color:#f8f8f8; }
      table td { border:1px solid blue; padding:3px; }
      ") (text . "") (html . "
      <button id=\"switchBtn\">Show full details</button><br/>
      <table><tbody id=\"movieList\"></tbody></table>
      ") (text . "")) ((text . "") (text . "Use a markup string as a nested template.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
      var movies = [
      { Name: \"The Red Violin\", Director: \"François Girard\" },
      { Name: \"Eyes Wide Shut\", Director: \"Stanley Kubrick\" },
      { Name: \"The Inheritance\", Director: \"Mauro Bolognini\" }
      ];
      
      /* Convert the markup string into a named template,
      referenced by the {{tmpl}} tag */
      $.template( \"titleTemplate\", \"<tr class='title'><td>${Name}</td></tr>\" );
      
      /* Render the movies data, using the named template as a nested template */
      $( \"#movieTemplate\" ).tmpl( movies ).appendTo( \"#movieList\" );
      ") (text . "") (css . "
      table { border-collapse:collapse; border:2px solid blue; margin:8px; background-color:#f8f8f8; }
      table tr { border:1px solid blue; } table td { padding:2px; }
      .title { border-bottom:none; } .detail { border-top:none; }
      ") (text . "") (html . "
      <tmpl id=\"movieTemplate\" type=\"text/x-jquery-tmpl\"> 
      {{tmpl \"titleTemplate\"}}
      <tr class=\"detail\"><td>Director: ${Director}</td></tr>
      </tmpl>
      
      <table><tbody id=\"movieList\"></tbody></table>
      ") (text . ""))))) jquery-doc-hash)

(push "$.template" jquery-methods)

(puthash "$.template" (quote (("name" . "$.template") ("signatures" "$.template" (("template" "The template markup to be compiled, or a string corresponding to a
named template.

" nil nil))) ("desc" (text . "Returns a compiled-template function.

")) ("longdesc" (text . "") (text . "Note: For information about how to render templates, see .tmpl() and
jQuery.tmpl().

") (text . "") (text . "If the template parameter is the name string for a named template
created using $.template( name, template ), this method returns the
compiled template for the named template (equivalent to
$.template[name]).
") (text . "") (text . "Otherwise, if the template parameter is a string containing HTML
markup, then this method will return a compiled template for the markup
provided.
") (text . "") (text . "If the template parameter is a string containing pure text (no HTML
tags), then the string is treated as a selector for an inline template,
whose content will be used as markup. Similarly if template is an HTML
element (or jQuery object wrapping an element), then the content will
be used as markup for the returned compiled template.
") (text . "") (text . "Example: Switch the template item to a different template, using
$.template( name ), :
   // Create the compiled templates
   $.template( \"summaryTemplate\", \"<tr>...</tr>\" );
   $.template( \"detailTemplate\", \"<tr>...</tr>\" );

   // Render the summaryTemplate with the \"movies\" data
   $.tmpl( \"summaryTemplate\", movies ).appendTo( \"#movieList\" );

   $( \"tr\" ).click( function () {
   // Switch the template for this template item to
   // a different named template, then update the rendered item
   var tmplItem = $.tmplItem(this);
   tmplItem.tmpl = $.template( \"detailTemplate\" );
   tmplItem.update();
   });

") (text . "")) ("examples" ((text . "") (text . "Dynamic switching of templates, using $.template() to obtain compiled
template.

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
    var movies = [
    { Name: \"The Red Violin\", ReleaseYear: \"1998\", Director: \"François Girard\" },
    { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\", Director: \"Stanley Kubrick\" },
    { Name: \"The Inheritance\", ReleaseYear: \"1976\", Director: \"Mauro Bolognini\" }
    ];
    
    var selectedItem = null;
    
    /* Create the compiled templates */
    $.template(
    \"summaryTemplate\",
    \"<tr class='movieSummary'><td colspan='2'>${Name}</td></tr>\"
    );
    $.template(
    \"detailTemplate\",
    \"<tr class='movieDetail row1'><td colspan='2'>${Name}</td></tr><tr class='movieDetail row2'><td>${ReleaseYear}</td><td>Director: ${Director}</td></tr>\"
    );
    
    /* Render the summaryTemplate with the \"movies\" data */ 
    $.tmpl( \"summaryTemplate\", movies ).appendTo( \"#movieList\" );
    
    $( \"#movieList\" )
    .delegate( \".movieSummary\", \"click\", function () {
    if (selectedItem) {
    /* Switch the template for this template item to
    the named template, then update the rendered item */
    selectedItem.tmpl = $.template( \"summaryTemplate\" );
    selectedItem.update();
    }
    selectedItem = $.tmplItem(this);
    /* Switch the template for this template item */
    selectedItem.tmpl = $.template( \"detailTemplate\" );
    selectedItem.update();
    })
    .delegate( \".movieDetail\", \"click\", function () {
    /* Switch the template for this template item */
    selectedItem.tmpl = $.template( \"summaryTemplate\" );
    selectedItem.update();
    selectedItem = null;
    });
    ") (text . "") (css . "
    table { cursor:pointer; border-collapse:collapse; 
    border:2px solid blue; width:300px; margin:8px; }
    table tr { border:1px solid blue; color:blue; background-color:#f8f8f8; } 
    table td { padding:3px; } table tr:hover { color:red; }
    .movieDetail { background-color:yellow; } 
    .movieDetail.row1 { border-bottom:none; } 
    .movieDetail.row2 { border-top:none; }
    ") (text . "") (html . "
    Click for details:
    <table><tbody id=\"movieList\"></tbody></table>
    ") (text . ""))))) jquery-doc-hash)

(push ".tmpl" jquery-methods)

(puthash ".tmpl" (quote (("name" . ".tmpl") ("signatures" ".tmpl" (("data" "The data to render. This can be any JavaScript type, including Array or
Object.

" "true" nil) ("options" "An optional map of user-defined key-value pairs. Extends the tmplItem
data structure, available to the template during rendering.

" "true" nil))) ("desc" (text . "Take the first element in the matched set and render its content as a
template, using the specified data.

")) ("longdesc" (text . "") (text . "The .tmpl() method is designed for chaining with .appendTo, .prependTo,
.insertAfter or .insertBefore as in the following example.

") (text . "") (text . "Example:


") (text . "") (js . "$( \"#myTemplate\" ).tmpl( myData ).appendTo( \"#target\" );") (text . "") (text . "If data is an array, the template is rendered once for each data item
in the array. If data is an object, or if the data parameter is missing
or null, a single template item is rendered.
") (text . "") (text . "The return value is a jQuery collection of elements made up of the
rendered template items (one for each data item in the array). If the
template contains only one top-level element, then there will be one
element for each data item in the array.
") (text . "") (text . "To insert the rendered template items into the HTML DOM, the returned
jQuery collection should not be inserted directly into the DOM, but
should be chained with .appendTo, .prependTo, .insertAfter or
.insertBefore, as in the example above:
") (text . "") (text . "See also jQuery.tmpl().


") (text . "") (text . "The following example shows how to use .tmpl() to render local data
using an inline template.

") (text . "") (js . "<ul id=\"movieList\"></ul>
      
      <script id=\"movieTemplate\" type=\"text/x-jquery-tmpl\">
      <li><b>${Name}</b> (${ReleaseYear})</li>
      </script>
      
      <script type=\"text/javascript\">
      var movies = [
      { Name: \"The Red Violin\", ReleaseYear: \"1998\" },
      { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\" },
      { Name: \"The Inheritance\", ReleaseYear: \"1976\" }
      ];
      
      // Render the template with the movies data and insert
      // the rendered HTML under the \"movieList\" element
      $( \"#movieTemplate\" ).tmpl( movies )
      .appendTo( \"#movieList\" );
      </script>
      ") (text . "") (text . " Using Remote Data


") (text . "") (text . "Typically the data is not local and is instead obtained using an Ajax
request to a remote service or page, as in the following example:

") (text . "") (js . "
	$.ajax({
	dataType: \"jsonp\",
	url: moviesServiceUrl,
	jsonp: \"$callback\",
	success: showMovies
	});
	
	// Within the callback, use .tmpl() to render the data.
	function showMovies( data ) {
	// Render the template with the \"movies\" data and insert
	// the rendered HTML under the 'movieList' element
	$( \"#movieTemplate\" ).tmpl( data )
        .appendTo( \"#movieList\" );
	}
      ") (text . "") (text . " The Container Element for the Template


") (text . "") (text . "You can get the markup for the template from inline markup in the page,
or from a string (possibly computed, or obtained remotely). For an
example of how to get the markup from a string, see jQuery.tmpl().
") (text . "") (text . "If a template is defined inline in the page, you can use $( selector
).tmpl( data ), where selector is a selector referencing the container
element that wraps the markup. The container can be any element, such
as a <div> element whose style attribute includes display:none.
However, this can result in invalid HTML or lead to side effects as a
result of the browser parsing the markup and loading it into the DOM.
Therefore, a preferred approach is to use a script tag such as <script
id=\"myContainer\" type=\"text/x-jquery-tmpl\"> to wrap the markup. For the
browser, the content will then be treated simply as text.
") (text . "") (text . " Caching the Template


") (text . "") (text . "When a template is rendered, the markup is first converted into a
compiled-template function. In the case of inline markup, calling $(
\"#myContainer\" ).tmpl( myData ) automatically causes the compiled
template to be cached. (The cached template is associated with the DOM
element that wraps the markup, using the jQuery .data() feature).
") (text . "") (text . "For convenience, you can also use $( \"#myContainer\" ).template( name )
so that you can reference the cached template by name. (See .template()
for examples).
") (text . "") (text . " Template Tags, Expressions, and Template Variables


") (text . "") (text . "Template tags such as the ${} tag can used within jQuery templates in
addition to text and HTML markup to enable a number of scenarios such
as composition of templates, iteration over hierarchical data,
parameterization of template rendering, etc. Template tags can render
content based on the values of data item fields or template variables
such as $item (corresponding to the template item), as well as
expressions and function calls. See the documentation topics for each
template tag: ${}, {{each}}, {{if}}, {{else}}, {{html}}, {{tmpl}} and
{{wrap}}.
") (text . "") (text . " The options Parameter, and Template Items


") (text . "") (text . "Each template item (the result of rendering a data item with the
template) is associated with a tmplItem data structure, which can be
accessed using jQuery.tmplItem() and .tmplItem(), or the $item template
variable. Any fields or anonomyous methods passed in with the options
parameter of .tmpl() will extend the tmplItem data structure, and so be
available to the template as in the following example:
") (text . "") (text . "     Code:


") (text . "") (js . "
	// Render the template with the movies data
	$( \"#movieTemplate\" ).tmpl( movies,
	{ 
        myValue: \"somevalue\",
        myMethod: function() { 
        return \"something\";
        } 
	} 
	).appendTo( \"#movieList\" );
      ") (text . "") (text . "

") (text . "") (text . "     Template:


") (text . "") (js . "
	<script id=\"movieTemplate\" type=\"text/x-jquery-tmpl\"> 
	<li>
        Some content: ${$item.myMethod()}.<br/>
        More content: ${$item.myValue}.
	</li>
	</script> 
      ") (text . "")) ("examples" ((text . "") (text . "Render local data using .tmpl().

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
      var movies = [
      { Name: \"The Red Violin\", ReleaseYear: \"1998\" },
      { Name: \"Eyes Wide Shut\", ReleaseYear: \"1999\" },
      { Name: \"The Inheritance\", ReleaseYear: \"1976\" }
      ];
      
      /* Render the template with the movies data and insert
      the rendered HTML under the \"movieList\" element */
      $( \"#movieTemplate\" ).tmpl( movies )
      .appendTo( \"#movieList\" );
      ") (text . "") (html . "
      <tmpl id=\"movieTemplate\" type=\"text/x-jquery-tmpl\"> 
      <li><b>${Name}</b> (${ReleaseYear})</li>
      </tmpl>
      
      <ul id=\"movieList\"></ul>
      ") (text . "")) ((text . "") (text . "Render data from a remote service using .tmpl().

") (text . "") (text . "

") (text . "") (text . "

") (text . "") (js . "
      function getMovies( genre, skip, top ) {
      $.ajax({
      dataType: \"jsonp\",
      url: \"http://odata.netflix.com/Catalog/Genres('\" + genre
      + \"')/Titles?$format=json&$skip=\"
      + skip + \"&$top=\" + top,
      jsonp: \"$callback\",
      success: function( data ) {
      /* Get the movies array from the data */
      var movies = data.d;
      
      /* Remove current set of movie template items */
      $( \"#movieList\" ).empty();
      
      /* Render the template with the movies data and insert
      the rendered HTML under the \"movieList\" element */
      $( \"#movieTemplate\" ).tmpl( movies )
      .appendTo( \"#movieList\" );
      }
      });
      }
      
      $( \"#cartoonsBtn\" ).click( function() {
      getMovies( \"Cartoons\", 0, 6 );
      });
      
      $( \"#dramaBtn\" ).click( function() {
      getMovies( \"Drama\", 0, 6 );
      });
      ") (text . "") (html . "
      <tmpl id=\"movieTemplate\" type=\"text/x-jquery-tmpl\"> 
      <li><b>${Name}</b> (${ReleaseYear})</li>
      </tmpl>
      
      <button id=\"cartoonsBtn\">Cartoons</button>
      <button id=\"dramaBtn\">Drama</button>
      
      <ul id=\"movieList\"></ul>
      ") (text . ""))))) jquery-doc-hash)

(provide (quote jquery-doc-data))
