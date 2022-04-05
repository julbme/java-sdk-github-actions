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
 * The class using {@link java.lang.System} functions to implement {@link SystemProxy}. <br>
 * @author Julb.
 */
class SystemProxySystemImpl implements SystemProxy {

    /**
     * {@inheritDoc}
     */
    @Override
    public void exit(int status) {
        System.exit(status);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings("java:S106")
    public void println() {
        System.out.println();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings("java:S106")
    public <T> void println(T name) {
        System.out.println(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getenv(String name) {
        return System.getenv(name);
    }
}
