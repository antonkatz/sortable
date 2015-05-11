/*
The MIT License (MIT)

Copyright (c) 2015 Anton Kats

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
package com.sortable.challenge

/**
 * Eliminates the need for a logging library in such simple application.
 */
object SimpleLogger {
  private val errorsOn = true
  private val warnOn = true
  private val debugOn = true

  def error(msg: String, args: String*) = log("Error", msg, args:_*)
  def error(throwable: Throwable, msg: String, args: String*) = {
    log("Error", msg, args:_*)
    throwable.printStackTrace()
  }
  def warn(msg: String, args: String*) = log("Warn", msg, args:_*)
  def debug(msg: String, args: String*) = log("Debug", msg, args:_*)

  private def log(level: String, msg: String, args: String*) = println({level + ": \t" + msg} format(args:_*))
}
