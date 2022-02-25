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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.concurrent.Callable;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Test class for {@link GitHubActionsKit} class. <br>
 * @author Julb.
 */
@ExtendWith(MockitoExtension.class)
class GitHubActionsKitTest {

    /**
     * A mock for system proxy.
     */
    @Mock
    private SystemProxy systemProxyMock;

    /**
     * The class under test.
     */
    private GitHubActionsKit gitHubActionsKit;

    /**
     * Called before each test.
     */
    @BeforeEach
    void setUp() {
        this.gitHubActionsKit = new GitHubActionsKit(this.systemProxyMock);
    }

    /**
     * Test method.
     */
    @Test
    void whenInstanceRequired_thenReturnInstance()
        throws Exception {
        assertThat(GitHubActionsKit.INSTANCE).isNotNull();
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getInput("test");
        assertThat(testInput).isPresent().contains("value");

        var testInput2 = this.gitHubActionsKit.getInput("test 2");
        assertThat(testInput2).isPresent().contains("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputAbsentNull_thenReturnEmpty()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        var testInput = this.gitHubActionsKit.getInput("test");
        assertThat(testInput).isEmpty();
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"  "})
    void whenGetInputAbsentNullEmptyBlankString_thenReturnEmpty(String absentEnvValue)
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(absentEnvValue);
        var testInput = this.gitHubActionsKit.getInput("test");
        assertThat(testInput).isEmpty();
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputTrimTruePresent_thenReturnValueTrimmed()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getInput("test", true);
        assertThat(testInput).isPresent().contains("value");

        var testInput2 = this.gitHubActionsKit.getInput("test 2", true);
        assertThat(testInput2).isPresent().contains("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputTrimFalsePresent_thenReturnValueNotTrimmed()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getInput("test", false);
        assertThat(testInput).isPresent().contains(" value ");

        var testInput2 = this.gitHubActionsKit.getInput("test 2", false);
        assertThat(testInput2).isPresent().contains("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputTrimNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getInput(null, false));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getRequiredInput("test");
        assertThat(testInput).isEqualTo("value");

        var testInput2 = this.gitHubActionsKit.getRequiredInput("test 2");
        assertThat(testInput2).isEqualTo("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getRequiredInput("test"));
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getRequiredInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputTrimTruePresent_thenReturnValueTrimmed()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getRequiredInput("test", true);
        assertThat(testInput).isEqualTo("value");

        var testInput2 = this.gitHubActionsKit.getRequiredInput("test 2", true);
        assertThat(testInput2).isEqualTo("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputTrimFalsePresent_thenReturnValueNotTrimmed()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getRequiredInput("test", false);
        assertThat(testInput).isEqualTo(" value ");

        var testInput2 = this.gitHubActionsKit.getRequiredInput("test 2", false);
        assertThat(testInput2).isEqualTo("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputTrimAbsent_thenThrowNoSuchElementException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getRequiredInput("test", true));
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputTrimNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getRequiredInput(null, false));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetBooleanInputPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" true ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("false");

        var testInput = this.gitHubActionsKit.getBooleanInput("test");
        assertThat(testInput).isPresent();
        assertThat(testInput.get()).isTrue();

        var testInput2 = this.gitHubActionsKit.getBooleanInput("test 2");
        assertThat(testInput2).isPresent();
        assertThat(testInput2.get()).isFalse();

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");

    }

    /**
     * Test method.
     */
    @Test
    void whenGetBooleanInputAbsent_thenReturnEmpty()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        var testInput = this.gitHubActionsKit.getBooleanInput("test");
        assertThat(testInput).isEmpty();
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetBooleanInputInvalid_thenThrowIllegalArgumentException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn("abcd");

        assertThrows(IllegalArgumentException.class, () -> this.gitHubActionsKit.getBooleanInput("test"));

        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetBooleanInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getBooleanInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredBooleanInputPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" true ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("false");

        var testInput = this.gitHubActionsKit.getRequiredBooleanInput("test");
        assertThat(testInput).isTrue();

        var testInput2 = this.gitHubActionsKit.getRequiredBooleanInput("test 2");
        assertThat(testInput2).isFalse();

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");

    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredBooleanInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getRequiredBooleanInput("test"));
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredBooleanInputInvalid_thenThrowIllegalArgumentException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn("abcd");

        assertThrows(IllegalArgumentException.class, () -> this.gitHubActionsKit.getRequiredBooleanInput("test"));

        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredBooleanInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getRequiredBooleanInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnumInputPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" green ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("BLUE");

        var testInput = this.gitHubActionsKit.getEnumInput("test", Color.class);
        assertThat(testInput).isPresent();
        assertThat(testInput.get()).isEqualByComparingTo(Color.GREEN);

        var testInput2 = this.gitHubActionsKit.getEnumInput("test 2", Color.class);
        assertThat(testInput2).isPresent();
        assertThat(testInput2.get()).isEqualByComparingTo(Color.BLUE);

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnumInputAbsent_thenReturnEmpty()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        var testInput = this.gitHubActionsKit.getEnumInput("test", Color.class);
        assertThat(testInput).isEmpty();
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnumInputInvalid_thenThrowIllegalArgumentException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn("abcd");

        assertThrows(IllegalArgumentException.class, () -> this.gitHubActionsKit.getEnumInput("test", Color.class));

        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnumInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getEnumInput(null, Color.class));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnumInputPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" green ");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("BLUE");

        var testInput = this.gitHubActionsKit.getRequiredEnumInput("test", Color.class);
        assertThat(testInput).isEqualTo(Color.GREEN);

        var testInput2 = this.gitHubActionsKit.getRequiredEnumInput("test 2", Color.class);
        assertThat(testInput2).isEqualTo(Color.BLUE);

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnumInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getRequiredEnumInput("test", Color.class));
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnumInputInvalid_thenThrowIllegalArgumentException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn("abcd");

        assertThrows(IllegalArgumentException.class, () -> this.gitHubActionsKit.getRequiredEnumInput("test", Color.class));

        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnumInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getRequiredEnumInput(null, Color.class));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value \nvalue2\n\n    \nvalue3\n");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getMultilineInput("test");
        assertThat(testInput).isPresent();
        assertThat(testInput.get()).containsExactly("value", "value2", "value3");

        var testInput2 = this.gitHubActionsKit.getInput("test 2");
        assertThat(testInput2).isPresent().contains("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputAbsent_thenReturnEmpty()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        var testInput = this.gitHubActionsKit.getMultilineInput("test");
        assertThat(testInput).isEmpty();
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getMultilineInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputTrimTruePresent_thenReturnValueTrimmed()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value \nvalue2\n\n    \nvalue3\n");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getMultilineInput("test", true);
        assertThat(testInput).isPresent();
        assertThat(testInput.get()).containsExactly("value", "value2", "value3");

        var testInput2 = this.gitHubActionsKit.getInput("test 2", true);
        assertThat(testInput2).isPresent().contains("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputTrimFalsePresent_thenReturnValueNotTrimmed()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value \nvalue2\n\n    \nvalue3\n");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getMultilineInput("test", false);
        assertThat(testInput).isPresent();
        assertThat(testInput.get()).containsExactly(" value ", "value2", "value3");

        var testInput2 = this.gitHubActionsKit.getInput("test 2");
        assertThat(testInput2).isPresent().contains("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputTrimAbsent_thenReturnEmpty()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        var testInput = this.gitHubActionsKit.getMultilineInput("test", true);
        assertThat(testInput).isEmpty();
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputTrimNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getMultilineInput(null, false));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value \nvalue2\n\n    \nvalue3\n");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getRequiredMultilineInput("test");
        assertThat(testInput).containsExactly("value", "value2", "value3");

        var testInput2 = this.gitHubActionsKit.getRequiredMultilineInput("test 2");
        assertThat(testInput2).containsExactly("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getRequiredMultilineInput("test"));
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getRequiredMultilineInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputTrimTruePresent_thenReturnValueTrimmed()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value \nvalue2\n\n    \nvalue3\n");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getRequiredMultilineInput("test", true);
        assertThat(testInput).containsExactly("value", "value2", "value3");

        var testInput2 = this.gitHubActionsKit.getRequiredMultilineInput("test 2", true);
        assertThat(testInput2).containsExactly("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputTrimFalsePresent_thenReturnValueNotTrimmed()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(" value \nvalue2\n\n    \nvalue3\n");
        when(this.systemProxyMock.getenv("INPUT_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getRequiredMultilineInput("test", false);
        assertThat(testInput).containsExactly(" value ", "value2", "value3");

        var testInput2 = this.gitHubActionsKit.getRequiredMultilineInput("test 2", false);
        assertThat(testInput2).containsExactly("value2");

        verify(this.systemProxyMock).getenv("INPUT_TEST");
        verify(this.systemProxyMock).getenv("INPUT_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineTrimInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        when(this.systemProxyMock.getenv("INPUT_TEST")).thenReturn(null);
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getRequiredMultilineInput("test", true));
        verify(this.systemProxyMock).getenv("INPUT_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineTrimInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getRequiredMultilineInput(null, false));
    }

    /**
     * Test method.
     */
    @Test
    void whenSetOutput_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.setOutput("variable", "value");
        verify(this.systemProxyMock).println("::set-output name=variable::value");
        reset(this.systemProxyMock);

        this.gitHubActionsKit.setOutput("variable", 123);
        verify(this.systemProxyMock).println("::set-output name=variable::123");
        reset(this.systemProxyMock);
    }

    /**
     * Test method.
     */
    @Test
    void whenSetOutputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.setOutput(null, "value"));
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.setOutput("variable", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenSetEmptyOutput_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.setEmptyOutput("variable");
        verify(this.systemProxyMock).println("::set-output name=variable::");
        reset(this.systemProxyMock);
    }

    /**
     * Test method.
     */
    @Test
    void whenSetEmptyOutputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.setEmptyOutput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenSetOptionalOutputPresent_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.setOptionalOutput("variable", Optional.of("value"));
        verify(this.systemProxyMock).println("::set-output name=variable::value");
        reset(this.systemProxyMock);
    }

    /**
     * Test method.
     */
    @Test
    void whenSetOptionalOutputEmpty_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.setOptionalOutput("variable", Optional.empty());
        verify(this.systemProxyMock).println("::set-output name=variable::");
        reset(this.systemProxyMock);
    }

    /**
     * Test method.
     */
    @Test
    void whenSetOptionalOutputNull_thenThrowNullPointerException()
        throws Exception {
        var emptyOpt = Optional.empty();
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.setOptionalOutput(null, emptyOpt));
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.setOptionalOutput("variable", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenSetCommandEchoEnabled_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.setCommandEcho(true);
        verify(this.systemProxyMock).println("::echo::on");

        this.gitHubActionsKit.setCommandEcho(false);
        verify(this.systemProxyMock).println("::echo::off");
    }

    /**
     * Test method.
     */
    @Test
    void whenStartGroup_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.startGroup("group name");
        verify(this.systemProxyMock).println("::group::group name");
    }

    /**
     * Test method.
     */
    @Test
    void whenStartGroupNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.startGroup(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenRunInGroup_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.group("group name", () -> {
            this.gitHubActionsKit.setCommandEcho(true);
        });
        verify(this.systemProxyMock).println("::group::group name");
        verify(this.systemProxyMock).println("::echo::on");
        verify(this.systemProxyMock).println("::endgroup::");
    }

    /**
     * Test method.
     */
    @Test
    void whenRunInGroupNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.group(null, () -> {
        }));
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.group("group name", (Runnable) null));
    }

    /**
     * Test method.
     */
    @Test
    void whenCallInGroup_thenPrintCommand()
        throws Exception {
        var result = this.gitHubActionsKit.group("group name", () -> {
            this.gitHubActionsKit.setCommandEcho(true);
            return 123;
        });
        assertThat(result).isEqualTo(123);
        verify(this.systemProxyMock).println("::group::group name");
        verify(this.systemProxyMock).println("::echo::on");
        verify(this.systemProxyMock).println("::endgroup::");
    }

    /**
     * Test method.
     */
    @Test
    void whenCallInGroupNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.group(null, () -> {
            return 123;
        }));
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.group("group name", (Callable<?>) null));
    }

    /**
     * Test method.
     */
    @Test
    void whenEndGroup_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.endGroup();
        verify(this.systemProxyMock).println("::endgroup::");
    }

    /**
     * Test method.
     */
    @Test
    void whenSaveState_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.saveState("variable", "value");
        verify(this.systemProxyMock).println("::save-state name=variable::value");

        this.gitHubActionsKit.saveState("variable", 123);
        verify(this.systemProxyMock).println("::save-state name=variable::123");
    }

    /**
     * Test method.
     */
    @Test
    void whenSaveStateNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.saveState(null, "value"));
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.saveState("variable", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetStatePresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("STATE_TEST")).thenReturn(" value ");
        when(this.systemProxyMock.getenv("STATE_TEST_2")).thenReturn("value2");

        var testInput = this.gitHubActionsKit.getState("test");
        assertThat(testInput).isPresent().contains(" value ");

        var testInput2 = this.gitHubActionsKit.getState("test 2");
        assertThat(testInput2).isPresent().contains("value2");

        verify(this.systemProxyMock).getenv("STATE_TEST");
        verify(this.systemProxyMock).getenv("STATE_TEST_2");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetStateNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getState(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetStateAbsent_thenReturnEmpty()
        throws Exception {
        when(this.systemProxyMock.getenv("STATE_TEST")).thenReturn(null);
        var testInput = this.gitHubActionsKit.getState("test");
        assertThat(testInput).isEmpty();
        verify(this.systemProxyMock).getenv("STATE_TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenSetSecret_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.setSecret("value");
        verify(this.systemProxyMock).println("::add-mask::value");
        reset(this.systemProxyMock);

        this.gitHubActionsKit.setSecret(123);
        verify(this.systemProxyMock).println("::add-mask::123");
        reset(this.systemProxyMock);
    }

    /**
     * Test method.
     */
    @Test
    void whenSetSecretNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.setSecret(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenIsDebug_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("RUNNER_DEBUG")).thenReturn("1");
        assertThat(this.gitHubActionsKit.isDebug()).isTrue();
        verify(this.systemProxyMock).getenv("RUNNER_DEBUG");
        reset(this.systemProxyMock);

        when(this.systemProxyMock.getenv("RUNNER_DEBUG")).thenReturn("2");
        assertThat(this.gitHubActionsKit.isDebug()).isFalse();
        verify(this.systemProxyMock).getenv("RUNNER_DEBUG");
        reset(this.systemProxyMock);

        when(this.systemProxyMock.getenv("RUNNER_DEBUG")).thenReturn(null);
        assertThat(this.gitHubActionsKit.isDebug()).isFalse();
        verify(this.systemProxyMock).getenv("RUNNER_DEBUG");
        reset(this.systemProxyMock);
    }

    /**
     * Test method.
     */
    @Test
    void whenFail_thenExitWithErrorCode()
        throws Exception {
        this.gitHubActionsKit.fail("fatal error");
        verify(this.systemProxyMock).println("::error::fatal error");
        verify(this.systemProxyMock).exit(1);
    }

    /**
     * Test method.
     */
    @Test
    void whenFailNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.fail(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenDebug_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.debug("some message");
        verify(this.systemProxyMock).println("::debug::some message");
    }

    /**
     * Test method.
     */
    @Test
    void whenDebugNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.debug(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenNotice_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.notice("some message");
        verify(this.systemProxyMock).println("::notice::some message");
    }

    /**
     * Test method.
     */
    @Test
    void whenNoticeNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.notice(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenNoticeProperties_thenPrintCommand()
        throws Exception {
        //@formatter:off
        var props = AnnotationProperties.builder()
            .title("title")
            .file("file.txt")
            .startLine(1)
            .endLine(1)
            .startColumn(1)
            .endColumn(3)
            .build();
        //@formatter:on
        this.gitHubActionsKit.notice("some message", Optional.of(props));
        verify(this.systemProxyMock).println("::notice col=1,endColumn=3,endLine=1,file=file.txt,line=1,title=title::some message");
    }

    /**
     * Test method.
     */
    @Test
    void whenNoticePropertiesNull_thenThrowNullPointerException()
        throws Exception {
        var emptyOptional = Optional.<AnnotationProperties> empty();
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.notice(null, emptyOptional));
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.notice("some message", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenWarning_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.warning("some message");
        verify(this.systemProxyMock).println("::warning::some message");
    }

    /**
     * Test method.
     */
    @Test
    void whenWarningNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.warning(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenWarningProperties_thenPrintCommand()
        throws Exception {
        //@formatter:off
        var props = AnnotationProperties.builder()
            .title("title")
            .file("file.txt")
            .startLine(1)
            .endLine(1)
            .startColumn(1)
            .endColumn(3)
            .build();
        //@formatter:on
        this.gitHubActionsKit.warning("some message", Optional.of(props));
        verify(this.systemProxyMock).println("::warning col=1,endColumn=3,endLine=1,file=file.txt,line=1,title=title::some message");
    }

    /**
     * Test method.
     */
    @Test
    void whenWarningPropertiesNull_thenThrowNullPointerException()
        throws Exception {
        var emptyOptional = Optional.<AnnotationProperties> empty();
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.warning(null, emptyOptional));
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.warning("some message", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenError_thenPrintCommand()
        throws Exception {
        this.gitHubActionsKit.error("some message");
        verify(this.systemProxyMock).println("::error::some message");
    }

    /**
     * Test method.
     */
    @Test
    void whenErrorNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.error(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenErrorProperties_thenPrintCommand()
        throws Exception {
        //@formatter:off
        var props = AnnotationProperties.builder()
            .title("title")
            .file("file.txt")
            .startLine(1)
            .endLine(1)
            .startColumn(1)
            .endColumn(3)
            .build();
        //@formatter:on
        this.gitHubActionsKit.error("some message", Optional.of(props));
        verify(this.systemProxyMock).println("::error col=1,endColumn=3,endLine=1,file=file.txt,line=1,title=title::some message");
    }

    /**
     * Test method.
     */
    @Test
    void whenErrorPropertiesNull_thenThrowNullPointerException()
        throws Exception {
        var emptyOptional = Optional.<AnnotationProperties> empty();
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.error(null, emptyOptional));
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.error("some message", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetGitHubEnvVarsBranch_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("GITHUB_REF")).thenReturn("refs/heads/feature-branch-1");
        when(this.systemProxyMock.getenv("GITHUB_REF_NAME")).thenReturn("feature-branch-1");
        when(this.systemProxyMock.getenv("GITHUB_REF_TYPE")).thenReturn("branch");
        when(this.systemProxyMock.getenv("GITHUB_REPOSITORY")).thenReturn("julbme/repo");
        when(this.systemProxyMock.getenv("GITHUB_SHA")).thenReturn("ffac537e6cbbf934b08745a378932722df287a53");
        when(this.systemProxyMock.getenv("GITHUB_RUN_ID")).thenReturn("12345678");
        when(this.systemProxyMock.getenv("GITHUB_API_URL")).thenReturn("https://api.github.com");

        assertThat(this.gitHubActionsKit.getGitHubRef()).isEqualTo("refs/heads/feature-branch-1");
        assertThat(this.gitHubActionsKit.getGitHubRefName()).isEqualTo("feature-branch-1");
        assertThat(this.gitHubActionsKit.getGitHubRefType()).isEqualTo("branch");
        assertThat(this.gitHubActionsKit.isGitHubRefTypeBranch()).isTrue();
        assertThat(this.gitHubActionsKit.isGitHubRefTypeTag()).isFalse();
        assertThat(this.gitHubActionsKit.getGitHubRepository()).isEqualTo("julbme/repo");
        assertThat(this.gitHubActionsKit.getGitHubSha()).isEqualTo("ffac537e6cbbf934b08745a378932722df287a53");
        assertThat(this.gitHubActionsKit.getGitHubAbbreviatedSha()).isEqualTo("ffac537");
        assertThat(this.gitHubActionsKit.getGitHubRunId()).isEqualTo("12345678");
        assertThat(this.gitHubActionsKit.getGitHubApiUrl()).isEqualTo("https://api.github.com");

        verify(this.systemProxyMock).getenv("GITHUB_REF");
        verify(this.systemProxyMock).getenv("GITHUB_REF_NAME");
        verify(this.systemProxyMock, times(3)).getenv("GITHUB_REF_TYPE");
        verify(this.systemProxyMock).getenv("GITHUB_REPOSITORY");
        verify(this.systemProxyMock, times(2)).getenv("GITHUB_SHA");
        verify(this.systemProxyMock).getenv("GITHUB_RUN_ID");
        verify(this.systemProxyMock).getenv("GITHUB_API_URL");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetGitHubEnvVarsTag_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("GITHUB_REF")).thenReturn("refs/tags/1.0.0");
        when(this.systemProxyMock.getenv("GITHUB_REF_NAME")).thenReturn("1.0.0");
        when(this.systemProxyMock.getenv("GITHUB_REF_TYPE")).thenReturn("tag");
        when(this.systemProxyMock.getenv("GITHUB_REPOSITORY")).thenReturn("julbme/repo");
        when(this.systemProxyMock.getenv("GITHUB_SHA")).thenReturn("ffac537e6cbbf934b08745a378932722df287a53");
        when(this.systemProxyMock.getenv("GITHUB_RUN_ID")).thenReturn("12345678");
        when(this.systemProxyMock.getenv("GITHUB_API_URL")).thenReturn("https://api.github.com");

        assertThat(this.gitHubActionsKit.getGitHubRef()).isEqualTo("refs/tags/1.0.0");
        assertThat(this.gitHubActionsKit.getGitHubRefName()).isEqualTo("1.0.0");
        assertThat(this.gitHubActionsKit.getGitHubRefType()).isEqualTo("tag");
        assertThat(this.gitHubActionsKit.isGitHubRefTypeBranch()).isFalse();
        assertThat(this.gitHubActionsKit.isGitHubRefTypeTag()).isTrue();
        assertThat(this.gitHubActionsKit.getGitHubRepository()).isEqualTo("julbme/repo");
        assertThat(this.gitHubActionsKit.getGitHubSha()).isEqualTo("ffac537e6cbbf934b08745a378932722df287a53");
        assertThat(this.gitHubActionsKit.getGitHubAbbreviatedSha()).isEqualTo("ffac537");
        assertThat(this.gitHubActionsKit.getGitHubRunId()).isEqualTo("12345678");
        assertThat(this.gitHubActionsKit.getGitHubApiUrl()).isEqualTo("https://api.github.com");

        verify(this.systemProxyMock).getenv("GITHUB_REF");
        verify(this.systemProxyMock).getenv("GITHUB_REF_NAME");
        verify(this.systemProxyMock, times(3)).getenv("GITHUB_REF_TYPE");
        verify(this.systemProxyMock).getenv("GITHUB_REPOSITORY");
        verify(this.systemProxyMock, times(2)).getenv("GITHUB_SHA");
        verify(this.systemProxyMock).getenv("GITHUB_RUN_ID");
        verify(this.systemProxyMock).getenv("GITHUB_API_URL");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetGitHubEnvVars_thenThrowNoSuchElementException()
        throws Exception {
        when(this.systemProxyMock.getenv("GITHUB_REF")).thenReturn(null);
        when(this.systemProxyMock.getenv("GITHUB_REF_NAME")).thenReturn(null);
        when(this.systemProxyMock.getenv("GITHUB_REF_TYPE")).thenReturn(null);
        when(this.systemProxyMock.getenv("GITHUB_REPOSITORY")).thenReturn(null);
        when(this.systemProxyMock.getenv("GITHUB_SHA")).thenReturn(null);
        when(this.systemProxyMock.getenv("GITHUB_RUN_ID")).thenReturn(null);
        when(this.systemProxyMock.getenv("GITHUB_API_URL")).thenReturn(null);

        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getGitHubRef());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getGitHubRefName());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getGitHubRefType());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.isGitHubRefTypeBranch());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.isGitHubRefTypeTag());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getGitHubRepository());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getGitHubSha());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getGitHubAbbreviatedSha());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getGitHubRunId());
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getGitHubApiUrl());

        verify(this.systemProxyMock).getenv("GITHUB_REF");
        verify(this.systemProxyMock).getenv("GITHUB_REF_NAME");
        verify(this.systemProxyMock, times(3)).getenv("GITHUB_REF_TYPE");
        verify(this.systemProxyMock).getenv("GITHUB_REPOSITORY");
        verify(this.systemProxyMock, times(2)).getenv("GITHUB_SHA");
        verify(this.systemProxyMock).getenv("GITHUB_RUN_ID");
        verify(this.systemProxyMock).getenv("GITHUB_API_URL");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnvPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("TEST")).thenReturn("value");
        var result = this.gitHubActionsKit.getEnv("TEST");
        assertThat(result).isPresent().contains("value");
        verify(this.systemProxyMock).getenv("TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnvAbsent_thenReturnEmpty()
        throws Exception {
        when(this.systemProxyMock.getenv("TEST")).thenReturn(null);
        assertThat(this.gitHubActionsKit.getEnv("TEST")).isEmpty();
        verify(this.systemProxyMock).getenv("TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnvNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getEnv(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnvPresent_thenReturnValue()
        throws Exception {
        when(this.systemProxyMock.getenv("TEST")).thenReturn("value");
        assertThat(this.gitHubActionsKit.getRequiredEnv("TEST")).isEqualTo("value");
        verify(this.systemProxyMock).getenv("TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnvAbsent_thenThrowNoSuchElementException()
        throws Exception {
        when(this.systemProxyMock.getenv("TEST")).thenReturn(null);
        assertThrows(NoSuchElementException.class, () -> this.gitHubActionsKit.getRequiredEnv("TEST"));
        verify(this.systemProxyMock).getenv("TEST");
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnvNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> this.gitHubActionsKit.getRequiredEnv(null));
    }
}
