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

import static com.github.stefanbirkner.systemlambda.SystemLambda.catchSystemExit;
import static com.github.stefanbirkner.systemlambda.SystemLambda.tapSystemOutNormalized;
import static com.github.stefanbirkner.systemlambda.SystemLambda.withEnvironmentVariable;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Test class for {@link SystemProxySystemImpl} class. <br>
 * @author Julb.
 */
class SystemProxySystemImplTest {

    /**
     * The class under test.
     */
    private SystemProxy systemProxy = new SystemProxySystemImpl();

    /**
     * Test method.
     */
    @Test
    void whenExit_thenInvokeSystemExit()
        throws Exception {
        var exitCode = catchSystemExit(() -> systemProxy.exit(1));
        assertThat(exitCode).isEqualTo(1);
    }

    /**
     * Test method.
     */
    @Test
    void whenPrintln_thenInvokeSystemPrintln()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            systemProxy.println();
        });
        assertThat(output).isEqualTo("\n");
    }

    /**
     * Test method.
     */
    @Test
    void whenPrintlnMessage_thenInvokeSystemPrintlnMessage()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            systemProxy.println("Hello World");
        });
        assertThat(output).isEqualTo("Hello World\n");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetenv_thenInvokeSystemGetenv()
        throws Exception {
        withEnvironmentVariable("INPUT", "value").execute(() -> {
            assertThat(this.systemProxy.getenv("INPUT")).isEqualTo("value");
            assertThat(this.systemProxy.getenv("INPUT2")).isNull();
        });
    }
}
