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

import java.util.Map;
import java.util.Optional;

import lombok.RequiredArgsConstructor;

/**
 * A command to issue in the GitHub action.
 * <P>
 * @author Julb.
 */
@RequiredArgsConstructor
class Command<P, M> {

    /**
     * The GitHub command string prefix.
     */
    private static final String CMD_STRING = "::";

    /**
     * The command name.
     */
    private final String name;

    /**
     * The properties associated to the command.
     */
    private final Optional<Map<String, P>> properties;

    /**
     * The message.
     */
    private final Optional<M> message;

    // ------------------------------------------ Constructors.

    // ------------------------------------------ Utility methods.

    // ------------------------------------------ Read methods.

    // ------------------------------------------ Write methods.

    // ------------------------------------------ Overridden methods.

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        var sb = new StringBuilder();
        // Add command.
        sb.append(CMD_STRING);
        sb.append(this.name);

        // Add properties
        this.properties.ifPresent(map -> {
            if (!map.isEmpty()) {
                var propertiesAsList = map.entrySet().stream().map(entry -> String.format("%s=%s", entry.getKey(), escapePropertyValue(Optional.ofNullable(entry.getValue())))).toList();
                sb.append(" ").append(String.join(",", propertiesAsList));
            }
        });

        // Add message
        sb.append(CMD_STRING);
        sb.append(escapeMessage(this.message));

        return sb.toString();
    }

    /**
     * Escapes the given message.
     * @param message the message value.
     * @return the message with special characters escaped.
     */
    private static <T> String escapePropertyValue(Optional<T> propertyValue) {
        return propertyValue.map(Object::toString).orElse("").replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A").replace(":", "%3A").replace(",", "%2C");
    }

    /**
     * Escapes the given message.
     * @param message the message value.
     * @return the message with special characters escaped.
     */
    private static <T> String escapeMessage(Optional<T> message) {
        return message.map(Object::toString).orElse("").replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A");
    }
}
