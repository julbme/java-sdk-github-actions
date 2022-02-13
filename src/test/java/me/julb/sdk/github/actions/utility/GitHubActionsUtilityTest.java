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

package me.julb.sdk.github.actions.utility;

import static com.github.stefanbirkner.systemlambda.SystemLambda.catchSystemExit;
import static com.github.stefanbirkner.systemlambda.SystemLambda.tapSystemOutNormalized;
import static com.github.stefanbirkner.systemlambda.SystemLambda.withEnvironmentVariable;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.debug;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.endGroup;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.error;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.fail;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getBooleanInput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getEnumInput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getEnv;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getGitHubAbbreviatedSha;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getGitHubApiUrl;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getGitHubRef;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getGitHubRefName;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getGitHubRefType;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getGitHubRepository;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getGitHubRunId;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getGitHubSha;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getInput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getMultilineInput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getRequiredBooleanInput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getRequiredEnumInput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getRequiredEnv;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getRequiredInput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getRequiredMultilineInput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.getState;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.group;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.isDebug;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.isGitHubRefTypeBranch;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.isGitHubRefTypeTag;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.notice;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.saveState;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.setCommandEcho;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.setOutput;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.setSecret;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.startGroup;
import static me.julb.sdk.github.actions.utility.GitHubActionsUtility.warning;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.concurrent.Callable;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledIfEnvironmentVariable;

/**
 * Test class for {@link GitHubActionsUtility} class. <br>
 * @author Julb.
 */
class GitHubActionsUtilityTest {

    /**
     * Test method.
     */
    @Test
    void whenGetInputPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value ").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getInput("test");
            assertThat(testInput).isPresent();
            assertThat(testInput).contains("value");

            var testInput2 = getInput("test 2");
            assertThat(testInput2).isPresent();
            assertThat(testInput2).contains("value2");
        });

    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputAbsent_thenReturnEmpty()
        throws Exception {
        var testInput = getInput("test");
        assertThat(testInput).isEmpty();
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputTrimAbsent_thenReturnEmpty()
        throws Exception {
        var testInput = getInput("test", true);
        assertThat(testInput).isEmpty();
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputTrimTruePresent_thenReturnValueTrimmed()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value ").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getInput("test", true);
            assertThat(testInput).isPresent();
            assertThat(testInput).contains("value");

            var testInput2 = getInput("test 2", true);
            assertThat(testInput2).isPresent();
            assertThat(testInput2).contains("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputTrimFalsePresent_thenReturnValueNotTrimmed()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value ").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getInput("test", false);
            assertThat(testInput).isPresent();
            assertThat(testInput).contains(" value ");

            var testInput2 = getInput("test 2", false);
            assertThat(testInput2).isPresent();
            assertThat(testInput2).contains("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetInputTrimNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getInput(null, false));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value ").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getRequiredInput("test");
            assertThat(testInput).isEqualTo("value");

            var testInput2 = getRequiredInput("test 2");
            assertThat(testInput2).isEqualTo("value2");
        });

    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        assertThrows(NoSuchElementException.class, () -> getRequiredInput("test"));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getRequiredInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputTrimTruePresent_thenReturnValueTrimmed()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value ").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getRequiredInput("test", true);
            assertThat(testInput).isEqualTo("value");

            var testInput2 = getRequiredInput("test 2", true);
            assertThat(testInput2).isEqualTo("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputTrimFalsePresent_thenReturnValueNotTrimmed()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value ").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getRequiredInput("test", false);
            assertThat(testInput).isEqualTo(" value ");

            var testInput2 = getRequiredInput("test 2", false);
            assertThat(testInput2).isEqualTo("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputTrimAbsent_thenThrowNoSuchElementException()
        throws Exception {
        assertThrows(NoSuchElementException.class, () -> getRequiredInput("test", true));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredInputTrimNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getRequiredInput(null, false));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetBooleanInputPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " true ").and("INPUT_TEST_2", "false").execute(() -> {
            var testInput = getBooleanInput("test");
            assertThat(testInput).isPresent();
            assertThat(testInput.get()).isTrue();

            var testInput2 = getBooleanInput("test 2");
            assertThat(testInput2).isPresent();
            assertThat(testInput2.get()).isFalse();
        });

    }

    /**
     * Test method.
     */
    @Test
    void whenGetBooleanInputAbsent_thenReturnEmpty()
        throws Exception {
        var testInput = getBooleanInput("test");
        assertThat(testInput).isEmpty();
    }

    /**
     * Test method.
     */
    @Test
    void whenGetBooleanInputInvalid_thenThrowIllegalArgumentException()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", "abcd").execute(() -> {
            assertThrows(IllegalArgumentException.class, () -> getBooleanInput("test"));
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetBooleanInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getBooleanInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredBooleanInputPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " true ").and("INPUT_TEST_2", "false").execute(() -> {
            var testInput = getRequiredBooleanInput("test");
            assertThat(testInput).isTrue();

            var testInput2 = getRequiredBooleanInput("test 2");
            assertThat(testInput2).isFalse();
        });

    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredBooleanInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        assertThrows(NoSuchElementException.class, () -> getRequiredBooleanInput("test"));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredBooleanInputInvalid_thenThrowIllegalArgumentException()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", "abcd").execute(() -> {
            assertThrows(IllegalArgumentException.class, () -> getRequiredBooleanInput("test"));
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredBooleanInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getRequiredBooleanInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnumInputPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " green ").and("INPUT_TEST_2", "BLUE").execute(() -> {
            var testInput = getEnumInput("test", Color.class);
            assertThat(testInput).isPresent();
            assertThat(testInput.get()).isEqualByComparingTo(Color.GREEN);

            var testInput2 = getEnumInput("test 2", Color.class);
            assertThat(testInput2).isPresent();
            assertThat(testInput2.get()).isEqualByComparingTo(Color.BLUE);
        });

    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnumInputAbsent_thenReturnEmpty()
        throws Exception {
        var testInput = getEnumInput("test", Color.class);
        assertThat(testInput).isEmpty();
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnumInputInvalid_thenThrowIllegalArgumentException()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", "abcd").execute(() -> {
            assertThrows(IllegalArgumentException.class, () -> getEnumInput("test", Color.class));
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnumInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getEnumInput(null, Color.class));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnumInputPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " green ").and("INPUT_TEST_2", "BLUE").execute(() -> {
            var testInput = getRequiredEnumInput("test", Color.class);
            assertThat(testInput).isEqualTo(Color.GREEN);

            var testInput2 = getRequiredEnumInput("test 2", Color.class);
            assertThat(testInput2).isEqualTo(Color.BLUE);
        });

    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnumInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        assertThrows(NoSuchElementException.class, () -> getRequiredEnumInput("test", Color.class));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnumInputInvalid_thenThrowIllegalArgumentException()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", "abcd").execute(() -> {
            assertThrows(IllegalArgumentException.class, () -> getRequiredEnumInput("test", Color.class));
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnumInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getRequiredEnumInput(null, Color.class));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value \nvalue2\n\n    \nvalue3\n").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getMultilineInput("test");
            assertThat(testInput).isPresent();
            assertThat(testInput.get()).containsExactly("value", "value2", "value3");

            var testInput2 = getInput("test 2");
            assertThat(testInput2).isPresent();
            assertThat(testInput2).contains("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputAbsent_thenReturnEmpty()
        throws Exception {
        var testInput = getMultilineInput("test");
        assertThat(testInput).isEmpty();
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getMultilineInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputTrimTruePresent_thenReturnValueTrimmed()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value \nvalue2\n\n    \nvalue3\n").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getMultilineInput("test", true);
            assertThat(testInput).isPresent();
            assertThat(testInput.get()).containsExactly("value", "value2", "value3");

            var testInput2 = getInput("test 2", true);
            assertThat(testInput2).isPresent();
            assertThat(testInput2).contains("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputTrimFalsePresent_thenReturnValueNotTrimmed()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value \nvalue2\n\n    \nvalue3\n").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getMultilineInput("test", false);
            assertThat(testInput).isPresent();
            assertThat(testInput.get()).containsExactly(" value ", "value2", "value3");

            var testInput2 = getInput("test 2");
            assertThat(testInput2).isPresent();
            assertThat(testInput2).contains("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputTrimAbsent_thenReturnEmpty()
        throws Exception {
        var testInput = getMultilineInput("test", true);
        assertThat(testInput).isEmpty();
    }

    /**
     * Test method.
     */
    @Test
    void whenGetMultilineInputTrimNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getMultilineInput(null, false));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value \nvalue2\n\n    \nvalue3\n").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getRequiredMultilineInput("test");
            assertThat(testInput).containsExactly("value", "value2", "value3");

            var testInput2 = getRequiredMultilineInput("test 2");
            assertThat(testInput2).containsExactly("value2");
        });

    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        assertThrows(NoSuchElementException.class, () -> getRequiredMultilineInput("test"));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getRequiredMultilineInput(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputTrimTruePresent_thenReturnValueTrimmed()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value \nvalue2\n\n    \nvalue3\n").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getRequiredMultilineInput("test", true);
            assertThat(testInput).containsExactly("value", "value2", "value3");

            var testInput2 = getRequiredMultilineInput("test 2", true);
            assertThat(testInput2).containsExactly("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineInputTrimFalsePresent_thenReturnValueNotTrimmed()
        throws Exception {
        withEnvironmentVariable("INPUT_TEST", " value \nvalue2\n\n    \nvalue3\n").and("INPUT_TEST_2", "value2").execute(() -> {
            var testInput = getRequiredMultilineInput("test", false);
            assertThat(testInput).containsExactly(" value ", "value2", "value3");

            var testInput2 = getRequiredMultilineInput("test 2", false);
            assertThat(testInput2).containsExactly("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineTrimInputAbsent_thenThrowNoSuchElementException()
        throws Exception {
        assertThrows(NoSuchElementException.class, () -> getRequiredMultilineInput("test", true));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredMultilineTrimInputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getRequiredMultilineInput(null, false));
    }

    /**
     * Test method.
     */
    @Test
    void whenSetOutput_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            setOutput("variable", "value");
        });
        assertEquals("::set-output name=variable::value\n", output);

        var outputNumber = tapSystemOutNormalized(() -> {
            setOutput("variable", 123);
        });
        assertEquals("::set-output name=variable::123\n", outputNumber);
    }

    /**
     * Test method.
     */
    @Test
    void whenSetOutputNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> setOutput(null, "value"));
        assertThrows(NullPointerException.class, () -> setOutput("variable", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenSetCommandEchoEnabled_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            setCommandEcho(true);
        });
        assertEquals("::echo::on\n", output);

        var outputNumber = tapSystemOutNormalized(() -> {
            setCommandEcho(false);
        });
        assertEquals("::echo::off\n", outputNumber);
    }

    /**
     * Test method.
     */
    @Test
    void whenStartGroup_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            startGroup("group name");
        });
        assertEquals("::group::group name\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenStartGroupNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> startGroup(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenRunInGroup_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            group("group name", () -> {
                setCommandEcho(true);
            });
        });
        assertEquals("::group::group name\n::echo::on\n::endgroup::\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenRunInGroupNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> group(null, () -> {
        }));
        assertThrows(NullPointerException.class, () -> group("group name", (Runnable) null));
    }

    /**
     * Test method.
     */
    @Test
    void whenCallInGroup_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            var result = group("group name", () -> {
                setCommandEcho(true);
                return 123;
            });
            assertThat(result).isEqualTo(123);
        });
        assertEquals("::group::group name\n::echo::on\n::endgroup::\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenCallInGroupNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> group(null, () -> {
            return 123;
        }));
        assertThrows(NullPointerException.class, () -> group("group name", (Callable<?>) null));
    }

    /**
     * Test method.
     */
    @Test
    void whenEndGroup_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            endGroup();
        });
        assertEquals("::endgroup::\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenSaveState_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            saveState("variable", "value");
        });
        assertEquals("::save-state name=variable::value\n", output);

        var outputNumber = tapSystemOutNormalized(() -> {
            saveState("variable", 123);
        });
        assertEquals("::save-state name=variable::123\n", outputNumber);
    }

    /**
     * Test method.
     */
    @Test
    void whenSaveStateNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> saveState(null, "value"));
        assertThrows(NullPointerException.class, () -> saveState("variable", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetStatePresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("STATE_TEST", " value ").and("STATE_TEST_2", "value2").execute(() -> {
            var testInput = getState("test");
            assertThat(testInput).isPresent();
            assertThat(testInput).contains(" value ");

            var testInput2 = getState("test 2");
            assertThat(testInput2).isPresent();
            assertThat(testInput2).contains("value2");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetStateNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getState(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetStateAbsent_thenReturnEmpty()
        throws Exception {
        var testInput = getState("test");
        assertThat(testInput).isEmpty();
    }

    /**
     * Test method.
     */
    @Test
    void whenSetSecret_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            setSecret("value");
        });
        assertEquals("::add-mask::value\n", output);

        var outputNumber = tapSystemOutNormalized(() -> {
            setSecret(123);
        });
        assertEquals("::add-mask::123\n", outputNumber);
    }

    /**
     * Test method.
     */
    @Test
    void whenSetSecretNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> setSecret(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenIsDebug_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("RUNNER_DEBUG", "1").execute(() -> {
            assertThat(isDebug()).isTrue();
        });
        withEnvironmentVariable("RUNNER_DEBUG", "2").execute(() -> {
            assertThat(isDebug()).isFalse();
        });
        assertThat(isDebug()).isFalse();
    }

    /**
     * Test method.
     */
    @Test
    void whenFail_thenExitWithErrorCode()
        throws Exception {
        var exitCode = catchSystemExit(() -> {
            var output = tapSystemOutNormalized(() -> {
                fail("fatal error");
            });

            // A message must be logged.
            assertThat(output).isEqualTo("::error::fatal error\n");
        });
        assertThat(exitCode).isEqualTo(1);
    }

    /**
     * Test method.
     */
    @Test
    void whenFailNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> fail(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenDebug_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            debug("some message");
        });
        assertEquals("::debug::some message\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenDebugNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> debug(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenNotice_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            notice("some message");
        });
        assertEquals("::notice::some message\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenNoticeNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> notice(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenNoticeProperties_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
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
            notice("some message", Optional.of(props));
        });
        assertEquals("::notice col=1,endColumn=3,endLine=1,file=file.txt,line=1,title=title::some message\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenNoticePropertiesNull_thenThrowNullPointerException()
        throws Exception {
        var emptyOptional = Optional.<AnnotationProperties> empty();
        assertThrows(NullPointerException.class, () -> notice(null, emptyOptional));
        assertThrows(NullPointerException.class, () -> notice("some message", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenWarning_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            warning("some message");
        });
        assertEquals("::warning::some message\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenWarningNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> warning(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenWarningProperties_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
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
            warning("some message", Optional.of(props));
        });
        assertEquals("::warning col=1,endColumn=3,endLine=1,file=file.txt,line=1,title=title::some message\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenWarningPropertiesNull_thenThrowNullPointerException()
        throws Exception {
        var emptyOptional = Optional.<AnnotationProperties> empty();
        assertThrows(NullPointerException.class, () -> warning(null, emptyOptional));
        assertThrows(NullPointerException.class, () -> warning("some message", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenError_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
            error("some message");
        });
        assertEquals("::error::some message\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenErrorNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> error(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenErrorProperties_thenPrintCommand()
        throws Exception {
        var output = tapSystemOutNormalized(() -> {
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
            error("some message", Optional.of(props));
        });
        assertEquals("::error col=1,endColumn=3,endLine=1,file=file.txt,line=1,title=title::some message\n", output);
    }

    /**
     * Test method.
     */
    @Test
    void whenErrorPropertiesNull_thenThrowNullPointerException()
        throws Exception {
        var emptyOptional = Optional.<AnnotationProperties> empty();
        assertThrows(NullPointerException.class, () -> error(null, emptyOptional));
        assertThrows(NullPointerException.class, () -> error("some message", null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetGitHubEnvVars_thenReturnValue()
        throws Exception {
        //@formatter:off
        withEnvironmentVariable("GITHUB_REF", "refs/heads/feature-branch-1")
            .and("GITHUB_REF_NAME", "feature-branch-1")
            .and("GITHUB_REF_TYPE", "branch")
            .and("GITHUB_REPOSITORY", "julbme/repo")
            .and("GITHUB_SHA", "ffac537e6cbbf934b08745a378932722df287a53")
            .and("GITHUB_RUN_ID", "12345678")
            .and("GITHUB_API_URL", "https://api.github.com")
            .execute(() -> {
                assertThat(getGitHubRef()).isEqualTo("refs/heads/feature-branch-1");
                assertThat(getGitHubRefName()).isEqualTo("feature-branch-1");
                assertThat(getGitHubRefType()).isEqualTo("branch");
                assertThat(isGitHubRefTypeBranch()).isTrue();
                assertThat(isGitHubRefTypeTag()).isFalse();
                assertThat(getGitHubRepository()).isEqualTo("julbme/repo");
                assertThat(getGitHubSha()).isEqualTo("ffac537e6cbbf934b08745a378932722df287a53");
                assertThat(getGitHubAbbreviatedSha()).isEqualTo("ffac537");
                assertThat(getGitHubRunId()).isEqualTo("12345678");
                assertThat(getGitHubApiUrl()).isEqualTo("https://api.github.com");
        });
        //@formatter:on

        //@formatter:off
        withEnvironmentVariable("GITHUB_REF", "refs/tags/1.0.0")
            .and("GITHUB_REF_NAME", "1.0.0")
            .and("GITHUB_REF_TYPE", "tag")
            .and("GITHUB_REPOSITORY", "julbme/repo")
            .and("GITHUB_SHA", "ffac537e6cbbf934b08745a378932722df287a53")
            .and("GITHUB_RUN_ID", "12345678")
            .and("GITHUB_API_URL", "https://api.github.com")
            .execute(() -> {
                assertThat(getGitHubRef()).isEqualTo("refs/tags/1.0.0");
                assertThat(getGitHubRefName()).isEqualTo("1.0.0");
                assertThat(getGitHubRefType()).isEqualTo("tag");
                assertThat(isGitHubRefTypeBranch()).isFalse();
                assertThat(isGitHubRefTypeTag()).isTrue();
                assertThat(getGitHubRepository()).isEqualTo("julbme/repo");
                assertThat(getGitHubSha()).isEqualTo("ffac537e6cbbf934b08745a378932722df287a53");
                assertThat(getGitHubAbbreviatedSha()).isEqualTo("ffac537");
                assertThat(getGitHubRunId()).isEqualTo("12345678");
                assertThat(getGitHubApiUrl()).isEqualTo("https://api.github.com");
        });
        //@formatter:on
    }

    /**
     * Test method.
     */
    @Test
    @DisabledIfEnvironmentVariable(named = "GITHUB_REF", matches = "^.+$", disabledReason = "should not be executed inside GitHub action")
    void whenGetGitHubEnvVars_thenThrowNoSuchElementException()
        throws Exception {
        assertThrows(NoSuchElementException.class, () -> getGitHubRef());
        assertThrows(NoSuchElementException.class, () -> getGitHubRefName());
        assertThrows(NoSuchElementException.class, () -> getGitHubRefType());
        assertThrows(NoSuchElementException.class, () -> getGitHubRepository());
        assertThrows(NoSuchElementException.class, () -> getGitHubSha());
        assertThrows(NoSuchElementException.class, () -> getGitHubAbbreviatedSha());
        assertThrows(NoSuchElementException.class, () -> getGitHubRunId());
        assertThrows(NoSuchElementException.class, () -> getGitHubApiUrl());
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnvPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("TEST", "value").execute(() -> {
            var result = getEnv("TEST");
            assertThat(result).isPresent();
            assertThat(result).contains("value");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnvAbsent_thenReturnEmpty()
        throws Exception {
        assertThat(getEnv("TEST")).isEmpty();
    }

    /**
     * Test method.
     */
    @Test
    void whenGetEnvNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getEnv(null));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnvPresent_thenReturnValue()
        throws Exception {
        withEnvironmentVariable("TEST", "value").execute(() -> {
            assertThat(getRequiredEnv("TEST")).isEqualTo("value");
        });
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnvAbsent_thenThrowNoSuchElementException()
        throws Exception {
        assertThrows(NoSuchElementException.class, () -> getRequiredEnv("TEST"));
    }

    /**
     * Test method.
     */
    @Test
    void whenGetRequiredEnvNull_thenThrowNullPointerException()
        throws Exception {
        assertThrows(NullPointerException.class, () -> getRequiredEnv(null));
    }
}
