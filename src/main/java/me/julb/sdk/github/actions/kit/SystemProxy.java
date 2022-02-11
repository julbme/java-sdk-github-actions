/**
 * MIT License
 *
 * Copyright (c) 2017-2022 Julb
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package me.julb.sdk.github.actions.kit;

/**
 * An interface proxyfying access to {@link java.lang.System} functions. <br>
 * @author Julb.
 */
interface SystemProxy {

    /**
     * Gets the value of the given environment variable name.
     * @param name the name of the environment variable.
     * @return the value of the environment variable, or <code>null</code> if not exists.
     */
    String getenv(String name);

    /**
     * Prints a line feed on STDOUT.
     */
    void println();

    /**
     * Prints a message on STDOUT.
     * @param <T> the message object type.
     * @param message the message to print.
     */
    <T> void println(T message);

    /**
     * Exits the program with the given code.
     * @param status the status code.
     */
    void exit(int status);
}
