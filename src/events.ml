(** Many objects in Node.js emit events: a net.Server emits an event
    each time a peer connects to it, a fs.readStream emits an event when
    the file is opened. All objects which emit events are instances of
    events.EventEmitter. You can access this module by doing:
    require("events");

    Typically, event names are represented by a camel-cased string,
    however, there aren't any strict restrictions on that, as any
    string will be accepted.

    Functions can then be attached to objects, to be executed when an
    event is emitted. These functions are called listeners. Inside a
    listener function, this refers to the EventEmitter that the
    listener was attached to.

    Inheriting from EventEmitter is no different from inheriting from any
    other constructor function. For example:

    'use strict';
    const util = require('util');
    const EventEmitter = require('events');

    function MyEventEmitter() {
    // Initialize necessary properties from `EventEmitter` in this instance
    EventEmitter.call(this);
    }

    // Inherit functions from `EventEmitter`'s prototype
    util.inherits(MyEventEmitter, EventEmitter); *)

open Nodejs_kit

class type event = object

end

(** When an EventEmitter instance experiences an error, the typical
    action is to emit an 'error' event. Error events are treated as a
    special case in Node.js. If there is no listener for it, then the
    default action is to print a stack trace and exit the program.

    All EventEmitters emit the event 'newListener' when new listeners
    are added and 'removeListener' when a listener is removed.*)
class type ['a] event_emitter = object

  method addListener :
    (Js.Unsafe.any -> unit) Js.callback -> 'a event_emitter Js.meth

  (** Adds a listener to the end of the listeners array for the
      specified event. No checks are made to see if the listener has
      already been added. Multiple calls passing the same combination of
      event and listener will result in the listener being added multiple
      times.*)
  method on :
    js_str ->
    'a Js.callback -> 'a event_emitter Js.meth

  (** Adds a one time listener for the event. This listener is invoked
      only the next time the event is fired, after which it is removed.*)
  method once :
    js_str ->
    (Js.Unsafe.any -> unit) Js.callback -> 'a event_emitter Js.meth

  (** Removes a listener from the listener array for the specified
      event. Caution: changes array indices in the listener array behind
      the listener.*)
  method removeListener :
    js_str ->
    (Js.Unsafe.any -> unit) Js.callback -> 'a event_emitter Js.meth

  method removeAllListeners_event : js_str -> unit Js.meth

  method removeAllListeners : unit -> unit Js.meth

  method setMaxListeners : int -> unit Js.meth

  method getMaxListeners : unit -> int Js.meth

  (** emitter.setMaxListeners(n) sets the maximum on a per-instance
      basis. This class property lets you set it for all EventEmitter
      instances, current and future, effective immediately. Use with care.

      Note that emitter.setMaxListeners(n) still has precedence over
      EventEmitter.defaultMaxListeners.*)
  method defaultMaxListeners : int Js.prop

end

let require () : 'a event_emitter Js.t =
  Nodejs_kit.require "events"
