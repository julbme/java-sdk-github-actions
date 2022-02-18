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

import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.concurrent.Callable;

import lombok.AccessLevel;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Kit methods for GitHub actions. <br>
 * @author Julb.
 */
@RequiredArgsConstructor(access = AccessLevel.PACKAGE)
public class GitHubActionsKit {

    /**
     * The public instance to use.
     */
    public static final GitHubActionsKit INSTANCE = new GitHubActionsKit(new SystemProxySystemImpl());

    /**
     * An interface proxifying {@link java.lang.System} calls.
     */
    private final SystemProxy systemProxy;

    /**
     * Gets the input with the given name.
     * @param name the input name.
     * @return the input value trimmed, or empty if input is not provided.
     */
    public Optional<String> getInput(@NonNull String name) {
        return getInput(name, true);
    }

    /**
     * Gets the input with the given name, with the option to trim the value.
     * @param name the input name.
     * @param trimValue <code>true</code> to trim the input value, <code>false</code> to let as-is.
     * @return the input value or empty if input is not provided.
     */
    public Optional<String> getInput(@NonNull String name, boolean trimValue) {
        var inputEnvProperty = "INPUT_" + name.replaceAll("\\s", "_").toUpperCase();
        return getEnv(inputEnvProperty).map(v -> {
            if (trimValue) {
                return v.trim();
            } else {
                return v;
            }
        });
    }

    /**
     * Gets the input with the given name.<br>
     * This method throws a NoSuchElementException if the input does not exist.
     * @param name the input name.
     * @return the input value.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getRequiredInput(@NonNull String name) {
        return getInput(name).orElseThrow();
    }

    /**
     * Gets the input with the given name.<br>
     * This method throws a NoSuchElementException if the input does not exist.
     * @param name the input name.
     * @param trimValue <code>true</code> to trim the input value, <code>false</code> to let as-is.
     * @return the input value or empty if input is not provided.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getRequiredInput(@NonNull String name, boolean trimValue) {
        return getInput(name, trimValue).orElseThrow();
    }

    /**
     * Gets the boolean input with the given name.
     * @param name the input name.
     * @return the input value trimmed, or empty if input is not provided.
     * @throws IllegalArgumentException if the value is not a valid boolean value.
     */
    public Optional<Boolean> getBooleanInput(@NonNull String name) {
        return getInput(name, true).map(s -> {
            var sUpperCase = s.toLowerCase();
            if (Boolean.TRUE.toString().equals(sUpperCase)) {
                return true;
            } else if (Boolean.FALSE.toString().equals(sUpperCase)) {
                return false;
            } else {
                throw new IllegalArgumentException(s);
            }
        });
    }

    /**
     * Gets the boolean input with the given name.<br>
     * This method throws a NoSuchElementException if the input does not exist.
     * @param name the input name.
     * @return the input value.
     * @throws NoSuchElementException if the object does not exist.
     * @throws IllegalArgumentException if the value is not a valid boolean value.
     */
    public Boolean getRequiredBooleanInput(@NonNull String name) {
        return getBooleanInput(name).orElseThrow();
    }

    /**
     * Gets the input value with the given name as an {@link Enum}.
     * @param <T> the enum object type.
     * @param name the input name.
     * @param enumClass the enum class.
     * @return the input value as an enum, or empty if input is not provided.
     * @throws IllegalArgumentException if the value is not a valid enum value.
     */
    public <T extends Enum<T>> Optional<T> getEnumInput(@NonNull String name, Class<T> enumClass) {
        return getInput(name, true).map(s -> {
            var sUpperCase = s.toUpperCase();
            return Enum.valueOf(enumClass, sUpperCase);
        });
    }

    /**
     * Gets the enum input value with the given name as an {@link Enum}.<br>
     * This method throws a NoSuchElementException if the input does not exist.
     * @param <T> the enum object type.
     * @param name the input name.
     * @param enumClass the enum class.
     * @return the input value as an enum, or empty if input is not provided.
     * @throws NoSuchElementException if the object does not exist.
     * @throws IllegalArgumentException if the value is not a valid enum value.
     */
    public <T extends Enum<T>> T getRequiredEnumInput(@NonNull String name, Class<T> enumClass) {
        return getEnumInput(name, enumClass).orElseThrow();
    }

    /**
     * Gets the multiline input with the given name.<br>
     * Each value is trimmed and blank values are not returned.
     * @param name the input name.
     * @return the multiline input value as an array, or empty if input is not provided.
     */
    public Optional<String[]> getMultilineInput(@NonNull String name) {
        return getMultilineInput(name, true);
    }

    /**
     * Gets the multiline input with the given name.<br>
     * Blank values are not returned.
     * @param name the input name.
     * @param trimValue <code>true</code> to trim the input value, <code>false</code> to let as-is.
     * @return the multiline input value as an array, or empty if input is not provided.
     */
    public Optional<String[]> getMultilineInput(@NonNull String name, boolean trimValue) {
        return getInput(name, trimValue).map(String::lines).map(stream -> stream.map(s -> trimValue ? s.trim() : s).filter(s -> !s.isBlank()).toArray(String[]::new));
    }

    /**
     * Gets the multiline input with the given name.<br>
     * <br>
     * This method throws a NoSuchElementException if the input does not exist.<br>
     * Each value is trimmed and blank values are not returned.
     * @param name the input name.
     * @return the multiline input value as an array.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String[] getRequiredMultilineInput(@NonNull String name) {
        return getMultilineInput(name).orElseThrow();
    }

    /**
     * Gets the multiline input with the given name.<br>
     * <br>
     * This method throws a NoSuchElementException if the input does not exist.<br>
     * Blank values are not returned.
     * @param name the input name.
     * @param trimValue <code>true</code> to trim the input value, <code>false</code> to let as-is.
     * @return the multiline input value as an array.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String[] getRequiredMultilineInput(@NonNull String name, boolean trimValue) {
        return getMultilineInput(name, trimValue).orElseThrow();
    }

    /**
     * Sets the given output variable.
     * @param <T> the output object type.
     * @param name the output variable name.
     * @param value the output variable value.
     */
    public <T> void setOutput(@NonNull String name, @NonNull T value) {
        issue("set-output", Map.of("name", name), value);
    }

    /**
     * Sets the given output variable with an empty value.
     * @param name the output variable name.
     */
    public void setEmptyOutput(@NonNull String name) {
        issue("set-output", Map.of("name", name));
    }

    /**
     * Enables or disables the echoing of commands into STDOUT for the rest of the step.<br>
     * Echoing is disabled by default if ACTIONS_STEP_DEBUG is not set.
     * @param enabled <code>true</code> to enable, <code>false</code> otherwise.
     */
    public void setCommandEcho(boolean enabled) {
        issue("echo", enabled ? "on" : "off");
    }

    /**
     * Starts a group with the given name.<br>
     * Output until the call to {@link #endGroup()} will be foldable.
     * @param name the name of the group.
     */
    public void startGroup(@NonNull String name) {
        issue("group", name);
    }

    /**
     * Executes the given {@link Runnable} in a group.
     * @param name the group name.
     * @param exec the {@link Runnable} to execute within the group.
     */
    public void group(@NonNull String name, @NonNull Runnable exec) {
        startGroup(name);
        try {
            exec.run();
        } finally {
            endGroup();
        }
    }

    /**
     * Executes the given callable in a group.
     * @param <T> the object type returned by the given {@link Callable}.
     * @param name the group name.
     * @param exec the {@link Callable} to execute within the group.
     * @return the result returned by the {@link Callable}.
     * @throws Exception propagate as-is the exception thrown by the {@link Callable}.
     */
    public <T> T group(@NonNull String name, @NonNull Callable<T> exec)
        throws Exception {
        startGroup(name);
        try {
            return exec.call();
        } finally {
            endGroup();
        }
    }

    /**
     * Ends a group.
     */
    public void endGroup() {
        issue("endgroup");
    }

    /**
     * Saves the state for current action, the state can only be retrieved by this action's post job execution.
     * @param <T> the state value object type.
     * @param name the state variable name.
     * @param value the state variable value.
     */
    public <T> void saveState(@NonNull String name, @NonNull T value) {
        issue("save-state", Map.of("name", name), value);
    }

    /**
     * Gets the value of an state set by this action's main execution.
     * @param name the state variable name.
     * @return the state value or empty if state is not set.
     */
    public Optional<String> getState(@NonNull String name) {
        var stateEnvProperty = "STATE_" + name.replaceAll("\\s", "_").toUpperCase();
        return getEnv(stateEnvProperty);
    }

    /**
     * Registers a secret which will get masked from logs.
     * @param <T> the secret value object type.
     * @param value the secret value.
     */
    public <T> void setSecret(@NonNull T value) {
        issue("add-mask", value);
    }

    /**
     * Returns <code>true</code> if DEBUG mode is enabled in the runner, <code>false</code> otherwise.
     * @return <code>true</code> if DEBUG mode is enabled in the runner, <code>false</code> otherwise.
     */
    public boolean isDebug() {
        return getEnv("RUNNER_DEBUG").map(v -> v.equals("1")).orElse(false);
    }

    /**
     * Prints a <code>DEBUG</code> message to user log.
     * @param message the message.
     */
    public void debug(@NonNull String message) {
        issue("debug", message);
    }

    /**
     * Prints a <code>NOTICE</code> message to user log.
     * @param message the message.
     */
    public void notice(@NonNull String message) {
        notice(message, Optional.empty());
    }

    /**
     * Prints a <code>NOTICE</code> message to user log.
     * @param message the message.
     * @param properties the annotations to add to the message.
     */
    public void notice(@NonNull String message, @NonNull Optional<AnnotationProperties> properties) {
        issue("notice", properties.map(AnnotationProperties::toMap).orElse(Map.of()), message);
    }

    /**
     * Prints a <code>WARNING</code> message to user log.
     * @param message the message.
     */
    public void warning(@NonNull String message) {
        warning(message, Optional.empty());
    }

    /**
     * Prints a <code>WARNING</code> message to user log.
     * @param message the message.
     * @param properties the annotations to add to the message.
     */
    public void warning(@NonNull String message, @NonNull Optional<AnnotationProperties> properties) {
        issue("warning", properties.map(AnnotationProperties::toMap).orElse(Map.of()), message);
    }

    /**
     * Prints a <code>ERROR</code> message to user log.
     * @param message the message.
     */
    public void error(@NonNull String message) {
        error(message, Optional.empty());
    }

    /**
     * Prints a <code>ERROR</code> message to user log with given annotation properties.
     * @param message the message.
     * @param properties the annotations to add to the message.
     */
    public void error(@NonNull String message, @NonNull Optional<AnnotationProperties> properties) {
        issue("error", properties.map(AnnotationProperties::toMap).orElse(Map.of()), message);
    }

    /**
     * Fails the process with the given message.
     * @param message the error message.
     */
    public void fail(@NonNull String message) {
        error(message);
        this.systemProxy.exit(1);
    }

    /**
     * Returns the owner and repository name. For example, <code>octocat/Hello-World</code>. See <code>GITHUB_REPOSITORY</code>.
     * @return the owner and repository name. For example, <code>octocat/Hello-World</code>. See <code>GITHUB_REPOSITORY</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getGitHubRepository() {
        return getEnv("GITHUB_REPOSITORY").orElseThrow();
    }

    /**
     * Returns the type of ref that triggered the workflow run. Valid values are <code>branch</code> or <code>tag</code>. See <code>GITHUB_REF_TYPE</code>.
     * @return the type of ref that triggered the workflow run. Valid values are <code>branch</code> or <code>tag</code>. See <code>GITHUB_REF_TYPE</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getGitHubRefType() {
        return getEnv("GITHUB_REF_TYPE").orElseThrow();
    }

    /**
     * Returns <code>true</code> if the type of ref that triggered the workflow run is <code>branch</code>. See <code>GITHUB_REF_TYPE</code>.
     * @return <code>true</code> if the type of ref that triggered the workflow run is <code>branch</code>. See <code>GITHUB_REF_TYPE</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public boolean isGitHubRefTypeBranch() {
        return getGitHubRefType().equalsIgnoreCase("branch");
    }

    /**
     * Returns <code>true</code> if the type of ref that triggered the workflow run is <code>tag</code>. See <code>GITHUB_REF_TYPE</code>.
     * @return <code>true</code> if the type of ref that triggered the workflow run is <code>tag</code>. See <code>GITHUB_REF_TYPE</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public boolean isGitHubRefTypeTag() {
        return getGitHubRefType().equalsIgnoreCase("tag");
    }

    /**
     * Returns the branch or tag ref that triggered the workflow run. See <code>GITHUB_REF</code>.
     * @return the branch or tag ref that triggered the workflow run. See <code>GITHUB_REF</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getGitHubRef() {
        return getEnv("GITHUB_REF").orElseThrow();
    }

    /**
     * Returns the branch or tag name that triggered the workflow run. See <code>GITHUB_REF_NAME</code>.
     * @return the branch or tag name that triggered the workflow run. See <code>GITHUB_REF_NAME</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getGitHubRefName() {
        return getEnv("GITHUB_REF_NAME").orElseThrow();
    }

    /**
     * Returns the commit SHA that triggered the workflow. See <code>GITHUB_SHA</code>.
     * @return the commit SHA that triggered the workflow. See <code>GITHUB_SHA</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getGitHubSha() {
        return getEnv("GITHUB_SHA").orElseThrow();
    }

    /**
     * Returns the commit abbreviated SHA that triggered the workflow. See <code>GITHUB_SHA</code>.
     * @return the commit abbreviated SHA that triggered the workflow. See <code>GITHUB_SHA</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getGitHubAbbreviatedSha() {
        var sha = getGitHubSha();
        return sha.substring(0, Math.min(7, sha.length()));
    }

    /**
     * Returns the workflow run identifier. See <code>GITHUB_RUN_ID</code>.
     * @return the workflow run identifier. See <code>GITHUB_RUN_ID</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getGitHubRunId() {
        return getEnv("GITHUB_RUN_ID").orElseThrow();
    }

    /**
     * Returns the GitHub API url. See <code>GITHUB_API_URL</code>.
     * @return the GitHub API url. See <code>GITHUB_API_URL</code>.
     * @throws NoSuchElementException if the object does not exist.
     */
    public String getGitHubApiUrl() {
        return getEnv("GITHUB_API_URL").orElseThrow();
    }

    /**
     * Gets the environment variable value.
     * @param name the environment variable name.
     * @return the value if the environment variable exists, or {@link Optional#empty()}.
     */
    public Optional<String> getEnv(@NonNull String name) {
        return Optional.ofNullable(this.systemProxy.getenv(name));
    }

    /**
     * Gets an environment variable value.
     * @param name the environment variable name.
     * @return the value if the environment variable exists, or {@link Optional#empty()}.
     */
    public String getRequiredEnv(@NonNull String name) {
        return getEnv(name).orElseThrow();
    }

    /**
     * Issues the command.
     * @param command the command to execute.
     */
    private void issue(String command) {
        issueCommand(command, Optional.empty(), Optional.empty());
    }

    /**
     * Issues the command with the message.
     * @param <M> the message object type.
     * @param command the command to execute.
     * @param message the message to attach.
     */
    private <M> void issue(String command, M message) {
        issueCommand(command, Optional.empty(), Optional.of(message));
    }

    /**
     * Issue the command with the properties.
     * @param <P> the property value object type.
     * @param command the command to execute.
     * @param properties the properties to attach.
     * @param message the message to attach.
     */
    private <P> void issue(String command, Map<String, P> properties) {
        issueCommand(command, Optional.of(properties), Optional.empty());
    }

    /**
     * Issue the command with the properties and the message.
     * @param <P> the property value object type.
     * @param <M> the message object type.
     * @param command the command to execute.
     * @param properties the properties to attach.
     * @param message the message to attach.
     */
    private <P, M> void issue(String command, Map<String, P> properties, M message) {
        issueCommand(command, Optional.of(properties), Optional.of(message));
    }

    /**
     * Implementation of the process to issue the command with the properties and the message.
     * @param <P> the property value object type.
     * @param <M> the message object type.
     * @param command the command to execute.
     * @param properties the properties to attach.
     * @param message the message to attach.
     */
    private <P, M> void issueCommand(String command, Optional<Map<String, P>> properties, Optional<M> message) {
        this.systemProxy.println(new ExecCommand<>(command, properties, message).toString());
    }

}
