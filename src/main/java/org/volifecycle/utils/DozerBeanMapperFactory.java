package org.volifecycle.utils;

import org.dozer.DozerBeanMapper;

/**
 * A factory for a DozerBeanMapper instance.
 * 
 * @author Idriss Neumann <idriss.neumann@capgemini.com>
 *
 */
public class DozerBeanMapperFactory {
    private static final DozerBeanMapper MAPPER = new DozerBeanMapper();

    /**
     * Private constructor.
     */
    private DozerBeanMapperFactory() {

    }

    /**
     * Return a single instance of DozerBeanMapper.
     * 
     * @return DozerBeanMapper
     */
    public static DozerBeanMapper getInstance() {
        return MAPPER;
    }

}
