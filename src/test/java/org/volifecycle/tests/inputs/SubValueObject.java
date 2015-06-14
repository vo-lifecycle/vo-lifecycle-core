package org.volifecycle.tests.inputs;

import java.io.Serializable;

/**
 * Sub value object class
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class SubValueObject implements Serializable {
    private static final long serialVersionUID = 1L;

    private String code;
    private String label;

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return code + "," + label;
    }
}
