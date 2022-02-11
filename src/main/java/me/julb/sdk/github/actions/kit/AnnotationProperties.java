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
import java.util.Optional;
import java.util.TreeMap;

import lombok.Builder;

/**
 * Optional properties which can be sent with logging commands (notice, error, and warning). <br>
 * @author Julb.
 */
@Builder
public class AnnotationProperties {

    //@formatter:off
     /**
     * A title for the annotation.
     * -- GETTER --
     * Getter for {@link #title} property.
     * @return the value.
     * -- SETTER --
     * Setter for {@link #title} property.
     * @param title the value to set.
     */
     //@formatter:on
    private String title;

    //@formatter:off
     /**
     * The path of the file for which the annotation should be created.
     * -- GETTER --
     * Getter for {@link #file} property.
     * @return the value.
     * -- SETTER --
     * Setter for {@link #file} property.
     * @param file the value to set.
     */
     //@formatter:on
    private String file;

    //@formatter:off
     /**
     * The start line for the annotation.
     * -- GETTER --
     * Getter for {@link #startLine} property.
     * @return the value.
     * -- SETTER --
     * Setter for {@link #startLine} property.
     * @param startLine the value to set.
     */
     //@formatter:on
    private Integer startLine;

    //@formatter:off
     /**
     * The end line for the annotation. Defaults to `startLine` when `startLine` is provided.
     * -- GETTER --
     * Getter for {@link #endLine} property.
     * @return the value.
     * -- SETTER --
     * Setter for {@link #endLine} property.
     * @param endLine the value to set.
     */
     //@formatter:on
    private Integer endLine;

    //@formatter:off
     /**
     * The start column for the annotation. Cannot be sent when `startLine` and `endLine` are different values.
     * -- GETTER --
     * Getter for {@link #startColumn} property.
     * @return the value.
     * -- SETTER --
     * Setter for {@link #startColumn} property.
     * @param startColumn the value to set.
     */
     //@formatter:on
    private Integer startColumn;

    //@formatter:off
     /**
     * The start column for the annotation. Cannot be sent when `startLine` and `endLine` are different values. Defaults to `startColumn` when `startColumn` is provided.
     * -- GETTER --
     * Getter for {@link #endColumn} property.
     * @return the value.
     * -- SETTER --
     * Setter for {@link #endColumn} property.
     * @param endColumn the value to set.
     */
     //@formatter:on
    private Integer endColumn;

    // ------------------------------------------ Utility methods.

    /**
     * Serializes the properties as a {@link Map}.
     * @return the properties as a {@link Map}.
     */
    public Map<String, String> toMap() {
        var map = new TreeMap<String, String>();
        map.put("title", title);
        map.put("file", file);
        map.put("line", Optional.ofNullable(startLine).map(Object::toString).orElse(null));
        map.put("endLine", Optional.ofNullable(endLine).map(Object::toString).orElse(null));
        map.put("col", Optional.ofNullable(startColumn).map(Object::toString).orElse(null));
        map.put("endColumn", Optional.ofNullable(endColumn).map(Object::toString).orElse(null));
        return map;
    }

    // ------------------------------------------ Read methods.

    // ------------------------------------------ Write methods.

    // ------------------------------------------ Overridden methods.
}
