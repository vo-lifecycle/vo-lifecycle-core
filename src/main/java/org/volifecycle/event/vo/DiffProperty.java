package org.volifecycle.event.vo;

import java.io.Serializable;

/**
 * Diff property class.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class DiffProperty implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * Value before change
     */
    private String beforeValue;

    /**
     * Value after change
     */
    private String afterValue;

    /**
     * Name of impacted property
     */
    private String propertyName;

    /**
     * Parent property name
     */
    private String parentPropertyName;

    /**
     * Type of diff (size, value)
     */
    private String type;

    /**
     * @return the beforeValue
     */
    public String getBeforeValue() {
        return beforeValue;
    }

    /**
     * @param beforeValue
     *            the beforeValue to set
     */
    public void setBeforeValue(String beforeValue) {
        this.beforeValue = beforeValue;
    }

    /**
     * @return the afterValue
     */
    public String getAfterValue() {
        return afterValue;
    }

    /**
     * @param afterValue
     *            the afterValue to set
     */
    public void setAfterValue(String afterValue) {
        this.afterValue = afterValue;
    }

    /**
     * @return the propertyName
     */
    public String getPropertyName() {
        return propertyName;
    }

    /**
     * @param propertyName
     *            the propertyName to set
     */
    public void setPropertyName(String propertyName) {
        this.propertyName = propertyName;
    }

    /**
     * @return the parentPropertyName
     */
    public String getParentPropertyName() {
        return parentPropertyName;
    }

    /**
     * @param parentPropertyName
     *            the parentPropertyName to set
     */
    public void setParentPropertyName(String parentPropertyName) {
        this.parentPropertyName = parentPropertyName;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }
}
