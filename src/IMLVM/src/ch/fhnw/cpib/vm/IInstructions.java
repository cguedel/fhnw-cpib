// Virtual Machine Java 2015, V01
// Edgar F.A. Lederer, FHNW and Uni Basel, 2015

package ch.fhnw.cpib.vm;

import ch.fhnw.cpib.vm.IVirtualMachine.ExecutionError;

// idea: the compiler should not need a reference to the VM object

public interface IInstructions {

    // non-executable form of instructions
    interface IInstr {
        IExecInstr toExecInstr(VirtualMachine vm);
    }

    // executable form of instructions
    interface IExecInstr extends IInstr {
        void execute() throws ExecutionError;
    }

    // stop instruction
    class Stop implements IInstr {
        public String toString() { return "Stop"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new StopExec();
        }
    }

    // stack instruction
    class Dup implements IInstr {
        public String toString() { return "Dup"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new DupExec();
        }
    }

    // routine operations

    class AllocBlock implements IInstr {
        protected int size;
        public AllocBlock(int size) { this.size= size; }
        public String toString() { return "AllocBlock(" + size + ")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new AllocBlockExec(size);
        }
    }

    class AllocStack implements IInstr {
        protected int maxSize;
        public AllocStack(int maxSize) { this.maxSize= maxSize; }
        public String toString() { return "AllocStack(" + maxSize + ")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new AllocStackExec(maxSize);
        }
    }

    class Call implements IInstr {
        protected int routAddress;
        public Call(int routAddress) { this.routAddress= routAddress; }
        public String toString() { return "Call(" + routAddress + ")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new CallExec(routAddress);
        }
    }

    class Return implements IInstr {
        protected int size;
        public Return(int size) { this.size= size; }
        public String toString() { return "Return(" + size + ")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new ReturnExec(size);
        }
    }

    // load immediate value (value -> stack)
    class LoadImInt implements IInstr {
        protected int value;
        public LoadImInt(int value) { this.value= value; }
        public String toString() { return "LoadImInt(" + value + ")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new LoadImIntExec(value);
        }
    }
    
    class LoadImRatio implements IInstr {
    	protected Ratio value;
    	public LoadImRatio(Ratio value) { this.value = value; }
    	public String toString() { return "LoadImRatio(" + value + ")"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new LoadImRatioExec(value);
    	}
    }

    // load address relative to frame pointer (address -> stack)
    class LoadAddrRel implements IInstr {
        protected int relAddress;
        public LoadAddrRel(int relAddress) { this.relAddress= relAddress; }
        public String toString() { return "LoadAddrRel(" + relAddress + ")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new LoadAddrRelExec(relAddress);
        }
    }

    // load instruction with address on stack
    // load (inside stack -> top of stack) operation
    class Deref implements IInstr {
        public String toString() { return "Deref"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new DerefExec();
        }
    }

    // store instruction with address on stack
    // store (top of stack -> inside stack) operation
    class Store implements IInstr {
        public String toString() { return "Store"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new StoreExec();
        }
    }

    // monadic instructions

    class NegInt implements IInstr {
        public String toString() { return "NegInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new NegIntExec();
        }
    }
    
    class NegRatio implements IInstr {
        public String toString() { return "NegRatio"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new NegRatioExec();
        }
    }
    
    class NegBool implements IInstr {
    	public String toString() { return "NegBool"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new NegBoolExec();
    	}
    }
    
    class NumRatio implements IInstr {
    	public String toString() { return "NumRatio"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new NumRatioExec();
    	}
    }
    
    class DenomRatio implements IInstr {
    	public String toString() { return "DenomRatio"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new DenomRatioExec();
    	}
    }
    
    class FloorRatio implements IInstr {
    	public String toString() { return "FloorRatio"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new FloorRatioExec();
    	}
    }
    
    class CeilRatio implements IInstr {
    	public String toString() { return "CeilRatio"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new CeilRatioExec();
    	}
    }
    
    class RoundRatio implements IInstr {
    	public String toString() { return "RoundRatio"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new RoundRatioExec();
    	}
    }

    // dyadic instructions

    class AddInt implements IInstr {
        public String toString() { return "AddInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new AddIntExec();
        }
    }
    
    class AddRatio implements IInstr {
    	public String toString() { return "AddRatio"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new AddRatioExec();
    	}
    }

    class SubInt implements IInstr {
        public String toString() { return "SubInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new SubIntExec();
        }
    }
    
    class SubRatio implements IInstr {
        public String toString() { return "SubRatio"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new SubRatioExec();
        }
    }

    class MultInt implements IInstr {
        public String toString() { return "MultInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new MultIntExec();
        }
    }
    
    class MultRatio implements IInstr {
        public String toString() { return "MultRatio"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new MultRatioExec();
        }
    }

    class DivTruncInt implements IInstr {
        public String toString() { return "DivTruncInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new DivTruncIntExec();
        }
    }
    
    class DivRatio implements IInstr {
        public String toString() { return "DivRatio"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new DivRatioExec();
        }
    }

    class ModTruncInt implements IInstr {
        public String toString() { return "ModTruncInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new ModTruncIntExec();
        }
    }

    class EqInt implements IInstr {
        public String toString() { return "EqInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new EqIntExec();
        }
    }
    
    class EqRatio implements IInstr {
    	public String toString() { return "EqRatio"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new EqRatioExec();
    	}
    }

    class NeInt implements IInstr {
        public String toString() { return "NeInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new NeIntExec();
        }
    }
    
    class NeRatio implements IInstr {
    	public String toString() { return "NeRatio"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new NeRatioExec();
        }
    }

    class GtInt implements IInstr {
        public String toString() { return "GtInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new GtIntExec();
        }
    }
    
    class GtRatio implements IInstr {
    	public String toString() { return "GtRatio"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new GtRatioExec();
    	}
    }

    class LtInt implements IInstr {
        public String toString() { return "LtInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new LtIntExec();
        }
    }
    
    class LtRatio implements IInstr {
        public String toString() { return "LtRatio"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new LtRatioExec();
        }
    }

    class GeInt implements IInstr {
        public String toString() { return "GeInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new GeIntExec();
        }
    }
    
    class GeRatio implements IInstr {
        public String toString() { return "GeRatio"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new GeRatioExec();
        }
    }

    class LeInt implements IInstr {
        public String toString() { return "LeInt"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new LeIntExec();
        }
    }
    
    class LeRatio implements IInstr {
        public String toString() { return "LeRatio"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new LeRatioExec();
        }
    }

    // jump instructions

    class UncondJump implements IInstr {
        protected int jumpAddr;
        public UncondJump(int jumpAddr) { this.jumpAddr= jumpAddr; }
        public String toString() { return "UncondJump(" + jumpAddr + ")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new UncondJumpExec(jumpAddr);
        }
    }

    class CondJump implements IInstr {
        protected int jumpAddr;
        public CondJump(int jumpAddr) { this.jumpAddr= jumpAddr; }
        public String toString() { return "CondJump(" + jumpAddr + ")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new CondJumpExec(jumpAddr);
        }
    }

    // input (input -> stack) and output (stack -> output) instructions

    class InputBool implements IInstr {
        protected String indicator;
        public InputBool(String indicator) { this.indicator= indicator; }
        public String toString() { return "InputBool(\"" + indicator + "\")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new InputBoolExec(indicator);
        }
    }

    class InputInt implements IInstr {
        protected String indicator;
        public InputInt(String indicator) { this.indicator= indicator; }
        public String toString() { return "InputInt(\"" + indicator + "\")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new InputIntExec(indicator);
        }
    }
    
    class InputRatio implements IInstr {
    	protected String indicator;
        public InputRatio(String indicator) { this.indicator= indicator; }
        public String toString() { return "InputRatio(\"" + indicator + "\")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new InputRatioExec(indicator);
        }
    }

    class OutputBool implements IInstr {
        protected String indicator;
        public OutputBool(String indicator) { this.indicator= indicator; }
        public String toString() { return "OutputBool(\"" + indicator + "\")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new OutputBoolExec(indicator);
        }
    }

    class OutputInt implements IInstr {
        protected String indicator;
        public OutputInt(String indicator) { this.indicator= indicator; }
        public String toString() { return "OutputInt(\"" + indicator + "\")"; }
        public IExecInstr toExecInstr(VirtualMachine vm) {
            return vm.new OutputIntExec(indicator);
        }
    }
    
    class OutputRatio implements IInstr {
    	protected String indicator;
    	public OutputRatio(String indicator) { this.indicator = indicator; }
    	public String toString() { return "OutputRatio(\"" + indicator + "\")"; }
    	public IExecInstr toExecInstr(VirtualMachine vm) {
    		return vm.new OutputRatioExec(indicator);
    	}
    }
}
